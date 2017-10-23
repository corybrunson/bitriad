#' @title Triad census for affiliation networks
#'   
#' @description Given an affiliation network, tally all actor triads by 
#'   isomorphism or other congruence class.
#'   
#' @details The \code{triad_census_*} functions implement the several triad 
#'   censuses described below. Each census is based on a congruence relation 
#'   among the triads in an affiliation network, and each function returns a 
#'   matrix (or, in the "simple" case, a vector) recording the number of triads 
#'   in each congruence class.
#'   
#'   The function \code{triad_census} supercedes 
#'   \code{\link[igraph]{triad_census}} but calls in case \code{graph} is not an
#'   affiliation network.
#'   
#' @template triadcensus
#'   

#' @name triad_census
#' @family triad census functions
#' @seealso Original \strong{igraph} functions: 
#'   \code{\link[igraph]{triad_census}}
#' @param graph An \strong{igraph} object, usually an affiliation network.
#' @param ... Additional arguments (currently \code{use.integer} and
#'   \code{verbose}) passed to the \code{method} function.
#' @param add.names Logical; whether to label the rows and columns of the output
#'   matrix.
#' @param scheme Character; the type of triad census to calculate, matched to 
#'   \code{"full"}, \code{"difference"} (also \code{"uniformity"}), 
#'   \code{"binary"} (also \code{"structural"}), or \code{"simple"}.
#' @param method Character; the triad census method to use. Currently only 
#'   \code{"batagelj_mrvar"} is implemented. \code{"projection"} calls an 
#'   inefficient but reliable implementation in R from the first package version
#'   that invokes the \code{\link{simple_triad_census}} of the 
#'   \code{\link{actor_projection}} of \code{graph}.
#' @param use.integer Logical; whether to use the \code{IntegerMatrix} class in
#'   \strong{Rcpp} rather than the default \code{NumericMatrix}.
#' @param verbose Logical; whether to display progress bars.
#' @return A matrix counts of triad congruence classes, with row indices 
#'   reflecting pairwise exclusive events and column indices reflecting 
#'   triadwise events.
#' @examples
#' data(women_clique)
#' (tc <- triad_census(women_clique, add.names = TRUE))
#' sum(tc) == choose(vcount(actor_projection(women_clique)), 3)
#' @export
triad_census <- function(graph, ..., add.names = TRUE) {
  if (!is_an(graph)) {
    if (is_simple(graph) & !is_directed(graph)) {
      return(simple_triad_census(graph = graph, add.names = add.names))
    } else {
      tc <- igraph::triad_census(graph = graph)
      if (add.names) {
        names(tc) <- c(
          "003",
          "012", "102", "021D", "021U", "021C",
          "111D", "111U",
          "030T", "030C",
          "201", "120D", "120U", "120C", "210",
          "300"
        )
      }
      return(tc)
    }
  } else {
    return(triad_census_an(graph = graph, ..., add.names = add.names))
  }
}

#' @rdname triad_census
#' @export
triad_census_an <- function(
  graph,
  scheme = "full",
  method = "batagelj_mrvar", ...,
  add.names = TRUE
) {
  stopifnot(is_an(graph))
  
  # type of census
  scheme <- match.arg(scheme, c("full",
                                "binary", "structural",
                                "difference", "uniformity",
                                "simple"))
  if (scheme == "full") {
    return(triad_census_full(graph = graph,
                             method = method, ...,
                             add.names = add.names))
  } else if (scheme %in% c("binary", "structural")) {
    return(triad_census_binary(graph = graph,
                               method = method, ...,
                               add.names = add.names))
  } else if (scheme %in% c("difference", "uniformity")) {
    return(triad_census_difference(graph = graph,
                                   method = method, ...,
                                   add.names = add.names))
  } else {
    simple_triad_census(graph)
  }
}

#' @rdname triad_census
#' @export
triad.census.an <- function(...) {
  .Deprecated("triad_census_an")
  triad_census_an(...)
}

#' @rdname triad_census
#' @export
triad_census_full <- function(
  graph,
  method = "batagelj_mrvar", ...,
  add.names = TRUE
) {
  
  # trivial case
  if (max(degree(graph, V(graph)$type)) <= 1) {
    tc <- matrix(choose(actor_count(graph), 3), nrow = 1, ncol = 1)
  } else {
    # method
    method <- match.arg(method, c("batagelj_mrvar", "projection"))
    triad_census_fun <- get(paste0("triad_census_full_", method))
    tc <- triad_census_fun(graph = graph, ...)
    tc <- fix_empty_triad_overflow(tc, choose(actor_count(graph), 3))
  }
  
  # annotation
  if (add.names) {
    colnames(tc) <- 0:(ncol(tc) - 1)
    rownames(tc) <- paste(
      "(",
      sapply(0:(nrow(tc) - 1),
             function(i) paste(index_partition(i), collapse = ",")),
      ")", sep = ""
    )
  }
  
  tc
}

#' @rdname triad_census
triad_census_full_batagelj_mrvar <- function(graph, use.integer = FALSE) {
  int_max <- .Machine$integer.max
  triad_count <- choose(actor_count(graph), 3)
  if (use.integer) {
    if (triad_count > int_max) {
      warning("Number of triads is greater than integer storage limit.")
    }
    tc <- triad_census_full_batagelj_mrvar_integer_C(
      el = as_edgelist(graph, names = FALSE),
      na = actor_count(graph)
    )
  } else {
    tc <- triad_census_full_batagelj_mrvar_numeric_C(
      el = as_edgelist(graph, names = FALSE),
      na = actor_count(graph)
    )
  }
  tc[1, 1] <- triad_count - sum(tc)
  return(tc)
}

#' @rdname triad_census
triad_census_full_projection <- function(
  graph,
  verbose = FALSE
) {
  
  # Drop trivial cases
  if (vcount(graph) == 0) return(matrix(0, nrow = 0, ncol = 0))
  # Create projection
  proj <- actor_projection(graph, name = NA)
  
  # Find maximum values of x and of w
  max.x <- max(E(proj)$weight)
  # Initialize matrix (overestimating the number of columns)
  C <- as.data.frame(matrix(0,
                            nrow = choose(max.x + 3, 3),
                            ncol = max.x + 1))
  
  # Tally one-tied triads
  ot <- oneTiedTriads(proj)
  # Insert the totals at the proper entries of C
  # (Aggregated, so no repeats, so no information loss)
  if (length(ot) > 0) C[sapply(ot$x, function(x) {
    partition_index(c(x, 0, 0))
  }) + 1, 1] <- ot$n
  if (verbose) message('One-tied triads tallied')
  
  # Tally two-tied triads
  tt <- twoTiedTriads(proj)
  # Insert the totals at the proper entries of C
  # (Aggregated, so no repeats, so no information loss)
  if (!is.null(tt)) C[sapply(1:dim(tt)[1], function(i) {
    partition_index(c(tt[i, 1], tt[i, 2], 0))
  }) + 1, 1] <- tt$n
  if (verbose) message('Two-tied triads tallied')
  
  # Tally triangles
  tht <- threeTiedTriads(graph, graph = proj)
  # If there are any...
  if (!is.null(tht)) {
    # Trim any unnecessary columns
    max.w <- max(tht$w)
    C <- C[, 1:(max.w + 1), drop = FALSE]
    # For each value of w:
    for(w in 0:max.w) {
      # Which rows have weight w?
      rs <- which(tht$w == w)
      # Insert the totals at the proper rows in column w + 1 of C
      # (No repeats, so no information loss)
      if (length(rs) > 0) C[sapply(rs, function(i) {
        partition_index(as.numeric(tht[i, 1:3])) + 1
      }), w + 1] <- tht$n[rs]
    }
  }
  if (verbose) message('Three-tied triads tallied')
  
  # The remaining triads share no secondary nodes; count them as empty
  # (No triads should have yet been counted as empty)
  C[1, 1] <- choose(vcount(proj), 3) - sum(C)
  # Reality check: The total triad tally should equal |V(proj)|-choose-3
  # (but only makes sense within range of 'numeric' accuracy)
  # http://stackoverflow.com/questions/8804779/
  # what-is-integer-overflow-in-r-and-how-can-it-happen
  stopifnot(sum(C) == choose(vcount(proj), 3))
  # Clear names
  colnames(C) <- NULL
  C <- as.matrix(C)
  attr(C, "dimnames") <- NULL
  C
}

#' @rdname triad_census
#' @export
triad_census_difference <- function(
  graph,
  method = "batagelj_mrvar", ...,
  add.names = TRUE
) {
  
  # trivial case
  if (max(degree(graph, V(graph)$type)) <= 1) {
    tc <- matrix(0, nrow = 4, ncol = 2)
    tc[1, 1] <- choose(actor_count(graph), 3)
  } else {
    # method
    method <- match.arg(method, c("batagelj_mrvar", "projection"))
    triad_census_fun <- get(paste0("triad_census_difference_", method))
    tc <- triad_census_fun(graph = graph, ...)
    tc <- fix_empty_triad_overflow(tc, choose(actor_count(graph), 3))
  }
  
  # annotation
  if (add.names) {
    dimnames(tc) <- list(
      paste0(
        "(",
        apply(expand.grid(0:1, 0:1, 0:1), 1, paste, collapse = ","),
        ")"
      ),
      0:1
    )
  }
  
  tc
}

#' @rdname triad_census
triad_census_difference_batagelj_mrvar <- function(graph, use.integer = FALSE) {
  int_max <- .Machine$integer.max
  triad_count <- choose(actor_count(graph), 3)
  if (use.integer) {
    if (triad_count > int_max) {
      warning("Number of triads is greater than integer storage limit.")
    }
    tc <- triad_census_difference_batagelj_mrvar_integer_C(
      el = as_edgelist(graph, names = FALSE),
      na = actor_count(graph)
    )
  } else {
    tc <- triad_census_difference_batagelj_mrvar_numeric_C(
      el = as_edgelist(graph, names = FALSE),
      na = actor_count(graph)
    )
  }
  tc[1, 1] <- triad_count - sum(tc)
  return(tc)
}

#' @rdname triad_census
triad_census_difference_projection <- function(
  graph
) {
  # Initialize the matrix and define the number of actors
  C <- matrix(0, nrow = 8, ncol = 2)
  n <- length(which(!V(graph)$type))
  # Trivial casess (not enough actors)
  if(n < 3) return(C)
  # Trivial case (no events)
  if((vcount(graph) - n) == 0) {
    C[1, 1] <- C[1, 1] + choose(n, 3)
    return(C)
  }
  
  # Create one-mode projection
  proj <- actor_projection(graph, name = NA)
  # Leverage one-mode triad census for zero- or one-edged triads
  C[1:2, 1] <- simple_triad_census(proj)[1:2]
  if(sum(C) == choose(n, 3)) return(C)
  
  # Tally two-tied triads
  tt <- twoTiedTriads(proj)
  if(!is.null(tt)) {
    # Classify as 0,1,0 (equal edge wts) or 0,1,1 (distinct edge wts)
    ed <- stats::aggregate(tt$n, by = list(tt$x == tt$y), FUN = sum)
    # Insert the totals at the proper entries of C
    if(nrow(ed) == 1) C[4 - ed$Group.1[1], 1] <- ed$x[1] else
      C[3:4, 1] <- ed$x[2 - ed$Group.1]
  }
  
  # Find all triangles in the projection
  t <- do.call(cbind, cliques(proj, 3, 3))
  # Vector of triad weights
  w <- sapply(1:ncol(t), function(j) {
    shareWeight(graph, V(proj)$name[c(t[1, j], t[2, j], t[3, j])])
  })
  # Classify and tally
  l <- sapply(1:ncol(t), function(j) {
    # Pairwise exclusive counts
    pw <- sort(c(edgeWeight(proj, c(t[1, j], t[2, j])),
                 edgeWeight(proj, c(t[2, j], t[3, j])),
                 edgeWeight(proj, c(t[1, j], t[3, j])))) - w[j]
    # Equal or distinct pairwise exclusive event counts
    ed <- c(0, pw[1:2]) < pw
    # Row index in C
    sum((2 ^ (2:0)) * ed)
  })
  # Store tallies in C
  C[5:8, 1] <- tabulate(l[w == 0] + 1, nbins = 8)[5:8]
  C[, 2] <- tabulate(l[w > 0] + 1, nbins = 8)
  
  # Return the matrix
  stopifnot(sum(C) == choose(n, 3))
  C
}

#' @rdname triad_census
#' @export
unif_triad_census <- function(graph) {
  .Deprecated("triad_census_difference")
  triad_census_difference(graph)
}

#' @rdname triad_census
#' @export
unif.triad.census <- function(graph) {
  .Deprecated("triad_census_difference")
  triad_census_difference(graph)
}

#' @rdname triad_census
#' @export
triad_census_binary <- function(
  graph,
  method = "batagelj_mrvar", ...,
  add.names = TRUE
) {
  
  # trivial case
  if (max(degree(graph, V(graph)$type)) <= 1) {
    tc <- matrix(0, nrow = 4, ncol = 2)
    tc[1, 1] <- choose(actor_count(graph), 3)
  } else {
    # method
    method <- match.arg(method, c("batagelj_mrvar", "projection"))
    triad_census_fun <- get(paste0("triad_census_binary_", method))
    tc <- triad_census_fun(graph = graph, ...)
  }
  
  # annotation
  if (add.names) {
    dimnames(tc) <- list(as.character(0:3), as.character(0:1))
  }
  
  tc
}

#' @rdname triad_census
triad_census_binary_batagelj_mrvar <- function(graph, use.integer = FALSE) {
  int_max <- .Machine$integer.max
  triad_count <- choose(actor_count(graph), 3)
  if (use.integer) {
    if (triad_count > int_max) {
      warning("Number of triads is greater than integer storage limit.")
    }
    tc <- triad_census_binary_batagelj_mrvar_integer_C(
      el = as_edgelist(graph, names = FALSE),
      na = actor_count(graph)
    )
  } else {
    tc <- triad_census_binary_batagelj_mrvar_numeric_C(
      el = as_edgelist(graph, names = FALSE),
      na = actor_count(graph)
    )
  }
  tc[1, 1] <- triad_count - sum(tc)
  return(tc)
}

#' @rdname triad_census
triad_census_binary_projection <- function(
  graph,
  verbose = FALSE
) {
  # Initialize the matrix and define the number of actors
  C <- matrix(0, nrow = 4, ncol = 2)
  n <- length(which(!V(graph)$type))
  # Trivial casess (not enough actors)
  if(n < 3) return(C)
  # Trivial case (no events)
  if((vcount(graph) - n) == 0) {
    C[1, 1] <- C[1, 1] + choose(n, 3)
    return(C)
  }
  
  # Create one-mode projection
  proj <- actor_projection(graph, name = NA)
  # Leverage one-mode triad census
  C[1:3, 1] <- simple_triad_census(proj)[1:3]
  if(sum(C) == choose(n, 3)) return(C)
  
  # Find all triangles in the projection
  t <- do.call(cbind, cliques(proj, 3, 3))
  # Vector of triad weights
  w <- sapply(1:ncol(t), function(j) {
    shareWeight(graph, V(proj)$name[c(t[1, j], t[2, j], t[3, j])])
  })
  w0 <- which(w == 0)
  C[4, 1] <- length(w0)
  
  # Restrict to triads with 3-actor events
  if (length(w0) > 0) {
    t <- t[, -w0]
    w <- w[-w0]
  }
  # Compute the number of actor pairs with exclusive events in each
  if (ncol(t) > 0) {
    l <- sapply(1:ncol(t), function(j) sum(c(
      edgeWeight(proj, c(t[1, j], t[2, j])),
      edgeWeight(proj, c(t[2, j], t[3, j])),
      edgeWeight(proj, c(t[1, j], t[3, j]))) > w[j]))
  }
  C[, 2] <- tabulate(l + 1, nbins = 4)
  # Return the matrix
  stopifnot(sum(C) == choose(n, 3))
  C
}

#' @rdname triad_census
#' @export
str_triad_census <- function(graph) {
  .Deprecated("triad_census_binary")
  triad_census_binary(graph)
}

#' @rdname triad_census
#' @export
structural.triad.census <- function(graph) {
  .Deprecated("triad_census_binary")
  triad_census_binary(graph)
}

#' @rdname triad_census
#' @export
simple_triad_census <- function(graph, add.names = TRUE) {
  if (is_an(graph)) graph <- actor_projection(graph)
  # Ensure that 'graph' is simple and undirected
  if (!is_simple(graph) | is_directed(graph)) {
    stop("'graph' is not simple and directed.")
  }
  # Use implemented triad census if it makes sense
  tc <- igraph::triad_census(as.directed(graph))
  if (sum(tc) == choose(vcount(graph), 3) & all(tc >= 0) & !is.nan(tc[1])) {
    tc <- tc[c(1, 3, 11, 16)]
    if (add.names) names(tc) <- 0:3
    return(tc)
  }
  # Initialize census and graph size
  n <- vcount(graph)
  # Across edges, tally nodes adjacent to 0, 1, or 2 of the endpoints
  edge_plus <- apply(as_edgelist(graph), 1, function(pair) {
    nbhd <- neighborhood(graph, 1, pair)
    cons <- length(unique(unlist(nbhd))) - 2
    tris <- length(do.call(intersect, nbhd)) - 2
    c(n - cons - 2, cons - tris, tris)
  })
  # Store 'tc' as row sums, correct for repeats, fill in empty triad count
  tc <- c(0, rowSums(edge_plus) / 1:3)
  tc[1] <- choose(n, 3) - sum(tc)
  if (add.names) names(tc) <- 0:3
  tc
}

#' @rdname triad_census
#' @export
simple.triad.census <- simple_triad_census

fix_empty_triad_overflow <- function(census, total) {
  census_total <- sum(census)
  if (census_total == total) return(census)
  census[1, 1] <- if (is.na(suppressWarnings(as.integer(total)))) {
    # convert to double to avoid introducing NAs
    as.numeric(census[1, 1]) - as.numeric(census_total) + total
  } else {
    census[1, 1] - census_total + total
  }
  census
}
