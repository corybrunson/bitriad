#' Affiliation network clustering coefficients
#' 
#' This function computes a given flavor of transitivity (triadic closure) on a 
#' given affiliation network. The calculations are performed locally. Each 
#' flavor is defined as a proportion of "wedges" that are "closed", for suitable
#' definitions of both terms. The function \code{transitivity_an} is a shell
#' that proceeds across actors and computes wedges using the provided 
#' \code{wedgeFun}. These functions count the "wedges", and among them the 
#' "closed" ones, centered at a given actor node in a given affiliation network.
#' The triads method \code{transitivity_an_triads} first classifies every triad
#' centered at each node. The appropriate formula then counts the wedges and
#' closed wedges at each. The method is slower for a single flavor but can be
#' used to produce multiple flavors with negligible additional computational
#' cost. The wedges method \code{transitivity_an_wedges} relies on a separate
#' "wedge function" for each statistic. The algorithm calls the appropriate
#' wedge function to run over the necessary wedge centers and return a wedge
#' count matrix, which is returned back into \code{transitivity_an} for
#' outputting.
#' 
#' @name transitivity-an
#' @param bigraph An affiliation network; see \code{is_an}.
#' @param type Character; the type of clustering coefficient (defaults to 
#'   "global").
#' @param vids A subset of actor node ids at which to evaluate the local 
#'   clustering coefficient.
#' @param wedgeFun The wedge function; overrides \code{flavor}.
#' @param flavor The flavor of transitivity to be used; overridden by 
#'   \code{wedgeFun}.
#' @param add.names Logical; whether to label the matrix rows and columns.
#' @param triads A matrix of centered triads.
#' @return If \code{type} is "global", the global clustering coefficient of the 
#'   network; if "local", the local clustering coefficients of the actors; 
#'   otherwise, a 2-column matrix, each row of which gives the number of wedges 
#'   and the number of closed wedges centered at each actor.
#' @export
transitivity_an <- function(
  bigraph,
  type = "global",
  wedgeFun,
  flavor,
  vids = which(!V(bigraph)$type),
  add.names = FALSE
) {
  if(vcount(bigraph) == 0) {
    if(type == "global") {
      return(NaN)
    } else if(type == "local") {
      return(NULL)
    } else return(matrix(NA, nrow = 0, ncol = 2))
  }
  # Check that nodes are actors
  stopifnot(all(!V(bigraph)$type[vids]))
  # If global or both, need to look at all vertices
  Qs <- if(type == "global") which(!V(bigraph)$type) else vids
  
  if(missing(wedgeFun)) {
    if(missing(flavor)) {
      stop("Need a wedge function or a flavor")
    } else {
      wedges <- transitivity_an_triads(bigraph, Qs, flavor)
    }
  } else {
    if(!missing(flavor))
      warning("Wedge function provided; overriding flavor")
    wedges <- transitivity_an_wedges(bigraph, Qs, wedgeFun)
  }
  
  # Return appropriate statistics
  if(mode(wedges) == "list") {
    do.call(cbind, lapply(wedges, wedgeReturn,
                          type = type, add.names = add.names))
  } else {
    wedgeReturn(wedges, type = type, add.names = add.names)
  }
}

#' @rdname transitivity-an
#' @export
transitivity_an_triads <- function(
  bigraph,
  vids = which(!V(bigraph)$type),
  flavor
) {
  # Data frame of quadruples (w,x,y,z) of triads centered at Qs
  triads <- centeredTriads(bigraph = bigraph, vids = vids)
  
  # Wedge and closed wedge counts for each triad
  if(length(flavor) == 1) {
    wedges <- triadWedges(triads, flavor = flavor)
  } else {
    wedges <- lapply(flavor, triadWedges, triads = triads)
  }
  wedges
}

#' @rdname transitivity-an
#' @export
transitivity_an_wedges <- function(
  bigraph,
  vids = which(!V(bigraph)$type),
  wedgeFun
) {
  .Deprecated("triadclosure_an")
  # Wedge and closed wedge counts at each node
  t(matrix(unlist(lapply(vids, function(v) {
    wedgeFun(bigraph, v)
  })), nrow = 2))
}

#' @rdname transitivity-an
#' @export
transitivity.an <- transitivity_an

#' @rdname transitivity-an
#' @export
transitivity.an.triads <- transitivity_an_triads

#' @rdname transitivity-an
#' @export
transitivity.an.wedges <- transitivity_an_wedges

centeredTriads <- function(bigraph, vids) {
  
  # Require consistent indexing (for interchangeability of actor ids below)
  if(!is_an(bigraph)) stop("'bigraph' must be an affiliation network")
  # Actor projection
  graph <- actor_projection(bigraph)
  
  # Neighborhoods (with starting node removed)
  n <- lapply(neighborhood(graph, order = 1, nodes = vids), function(x) x[-1])
  
  # Across all nodes in vids...
  do.call(rbind, lapply(1:length(vids), function(i) {
    
    v <- vids[i]
    
    # Pairs of v's neighbors
    # Note: using nodes from bigraph requires consistent indexing
    # (guaranteed by is_an)
    #n <- setdiff(neighborhood(graph, order = 1, nodes = v)[[1]], v)
    ns <- utils::combn(n[[i]], m = 2)
    
    # Across all pairs of v's neighbors...
    dat <- cbind(v = v, do.call(rbind, lapply(1:ncol(ns), function(j) {
      
      # Triad class, unsorted, with v at the center
      triad_class(bigraph,
                  c(ns[1, j], v, ns[2, j]),
                  as.partition = FALSE, format = "vector")
    })))
    #colnames(dat)[3:5] <- c("x", "y", "z")
    dat
  }))
}

triadWedges <- function(triads, flavor) {
  
  # Extract w, x, y, and z from triads
  w <- triads[, "w"]
  x <- triads[, "x"]; y <- triads[, "y"]; z <- triads[, "z"]
  
  # Match the flavor to one of the (eventual) 9 natives
  flavor <- match.arg(flavor,
                      c("project", "watts.strogatz", "opsahl", "excl",
                        paste0("liebig.rao.", 0:3),
                        apply(expand.grid(c("hom", "inj", "ind"),
                                          c("equ", "str", "act")),
                              1, paste, collapse = "")))
  
  # Count wedges and their closures
  if(flavor == "homequ") {
    wedges <- x * y + (x + y) * w + w^2
    closed <- x * y * (w + z > 0) + (x + y) * w * (w + z > 0) + w^2
  } else if(flavor %in% c("opsahl", "injequ")) {
    wedges <- x * y + (x + y) * w
    closed <- x * y * (w + z > 0) + (x + y) * w * (w - 1 + z > 0)
  } else if(flavor %in% c("liebig.rao.0", "indequ")) {
    wedges <- x * y
    closed <- x * y * (z > 0)
  } else if(flavor == "homstr") {
    wedges <- (x>0)*(y>0) + ((x>0) + (y>0))*(w>0) + (w>0)
    closed <- (x>0)*(y>0)*(w+z>0) + ((x>0) + (y>0))*(w>0) + (w>0)
  } else if(flavor == "injstr") {
    wedges <- (x>0)*(y>0) + ((x>0) + (y>0))*(w>0) + (w>1)
    closed <- (x>0)*(y>0)*(w+z>0) + ((x>0) + (y>0))*(w>0)*(z>0|w>1) +
      (w>1)*(z>0|w>2)
  } else if(flavor %in% c("excl", "indstr", "indact")) {
    wedges <- (x > 0) * (y > 0)
    closed <- (x > 0) * (y > 0) * (z > 0)
  } else if(flavor %in% c("project", "watts.strogatz", "homact")) {
    wedges <- (w > 0) | (x > 0 & y > 0)
    closed <- (w > 0) | (x > 0 & y > 0 & z > 0)
  } else if(flavor == "injact") {
    wedges <- (x > 0 & y > 0) | ((x > 0 | y > 0) & w > 0) | (w > 1)
    closed <- (x > 0 & y > 0) * (w + z > 0) |
      ((x > 0 | y > 0) & w > 0) * (w > 1 | z > 0) |
      (w > 1) * (w > 2 | z > 0)
  } else if(flavor == "liebig.rao.1") {
    wedges <- x * y + (x + y) * w
    closed <- x * y * w + (x + y) * w * z
  } else if(flavor == "liebig.rao.2") {
    wedges <- (x + y) * w + w * (w - 1)
    closed <- (x + y) * w * (w - 1) + w * (w - 1) * z
  } else if(flavor == "liebig.rao.3") {
    wedges <- w * (w - 1)
    closed <- w * (w - 1) * (w - 2)
  } else {
    stop(paste("First and foremost, Farley Flavors Fabulous Fast Food",
               "feeds and fortifies families for a fabulous future!"))
  }
  
  # Aggregate wedges and closures over vertex ids
  mat <- as.matrix(stats::aggregate(
    data.frame(Wedges = wedges, Closed = closed),
    by = list(v = triads[, "v"]), FUN = sum
  )[, 2:3])
  #rownames(dat) <- triads[, "v"]
  mat
}

#' @rdname transitivity-an
#' @export
indequ_transitivity <- function(
  bigraph, type = "global",
  vids = which(!V(bigraph)$type)
) {
  transitivity_an(
    bigraph = bigraph, type = type,
    wedgeFun = indequ_wedges, vids = vids)
}

#' @rdname transitivity-an
#' @export
indequ.transitivity <- indequ_transitivity

#' @rdname transitivity-an
#' @export
indstr_transitivity <- function(
  bigraph, type = "global",
  vids = which(!V(bigraph)$type)
) {
  transitivity_an(
    bigraph = bigraph, type = type,
    wedgeFun = indstr_wedges, vids = vids)
}

#' @rdname transitivity-an
#' @export
indstr.transitivity <- indstr_transitivity

#' @rdname transitivity-an
#' @export
injact_transitivity <- function(
  bigraph, type = "global",
  vids = which(!V(bigraph)$type)
) {
  transitivity_an(
    bigraph = bigraph, type = type,
    wedgeFun = injact_wedges, vids = vids)
}

#' @rdname transitivity-an
#' @export
injact.transitivity <- injact_transitivity

#' @rdname transitivity-an
#' @export
injequ_transitivity <- function(
  bigraph, type = "global",
  vids = which(!V(bigraph)$type)
) {
  transitivity_an(
    bigraph = bigraph, type = type,
    wedgeFun = injequ_wedges, vids = vids)
}

#' @rdname transitivity-an
#' @export
injequ.transitivity <- injequ_transitivity

#' @rdname transitivity-an
#' @export
injstr_transitivity <- function(
  bigraph, type = "global",
  vids = which(!V(bigraph)$type)
) {
  transitivity_an(
    bigraph = bigraph, type = type,
    wedgeFun = injstr_wedges, vids = vids)
}

#' @rdname transitivity-an
#' @export
injstr.transitivity <- injstr_transitivity

#' @rdname transitivity-an
#' @export
opsahl_transitivity <- function(
  bigraph, type = "global",
  vids = which(!V(bigraph)$type)
) {
  transitivity_an(
    bigraph = bigraph, type = type,
    wedgeFun = injequ_wedges, vids = vids)
}

#' @rdname transitivity-an
#' @export
opsahl.transitivity <- opsahl_transitivity

#' @rdname transitivity-an
#' @export
excl_transitivity <- function(
  bigraph, type = "global",
  vids = which(!V(bigraph)$type)
) {
  transitivity_an(
    bigraph = bigraph, type = type,
    wedgeFun = indstr_wedges, vids = vids)
}

#' @rdname transitivity-an
#' @export
excl.transitivity <- excl_transitivity
