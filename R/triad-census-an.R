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
#' @template triadcensus
#'   

#' @name triad_census_an
#' @param bigraph An affiliation network.
#' @param scheme Character; the type of triad census to calculate, matched to 
#'   \code{"full"}, \code{"binary"} (also \code{"structural"}), 
#'   \code{"difference"} (also \code{"uniformity"}), or \code{"simple"}.
#' @param method Character; the triad census method to use. Currently only 
#'   \code{"batagelj_mrvar"} is implemented. \code{"projection"} calls an 
#'   inefficient but reliable implementation in R from the first package version
#'   that invokes the \code{\link{simple_triad_census}} of the 
#'   \code{\link{actor_projection}} of \code{bigraph}.
#' @param ... Additional arguments passed to the \code{method} function.
#' @param add.names Logical; whether to label the rows and columns of the output
#'   matrix.
#' @param verbose Logical; whether to display progress bars.
#' @return A matrix counts of triad congruence classes, with row indices 
#'   reflecting pairwise exclusive events and column indices reflecting 
#'   triadwise events.
#' @export
#' @examples
#' data(women_clique)
#' (tc <- triad_census_an(women_clique, add.names = TRUE))
#' sum(tc) == choose(vcount(actor_projection(women_clique)), 3)
triad_census_an <- function(
  bigraph,
  scheme = "full",
  method = "batagelj_mrvar", ...,
  add.names = FALSE
) {
  stopifnot(is_an(bigraph))
  
  # type of census
  scheme <- match.arg(scheme, c("full",
                                "binary", "structural",
                                "difference", "uniformity",
                                "simple"))
  if (scheme == "full") {
    return(triad_census_full(bigraph = bigraph,
                             method = method, ...,
                             add.names = add.names))
  } else if (scheme %in% c("binary", "structural")) {
    return(triad_census_binary(bigraph = bigraph,
                               method = method, ...,
                               add.names = add.names))
  } else if (scheme %in% c("difference", "uniformity")) {
    return(triad_census_difference(bigraph = bigraph,
                                   method = method, ...,
                                   add.names = add.names))
  } else {
    simple_triad_census(actor_projection(bigraph))
  }
}

#' @rdname triad_census_an
#' @export
triad.census.an <- triad_census_an

#' @rdname triad_census_an
#' @export
triad_census_full <- function(
  bigraph,
  method = "batagelj_mrvar", ...,
  add.names = FALSE
) {
  
  # trivial case
  if (max(degree(bigraph, V(bigraph)$type)) <= 1) {
    tc <- matrix(choose(actor_count(bigraph), 3), nrow = 1, ncol = 1)
  } else {
    # method
    method <- match.arg(method, c("batagelj_mrvar", "projection"))
    triad_census_fun <- get(paste0("triad_census_", method))
    tc <- triad_census_fun(bigraph = bigraph, ...)
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

#' @rdname triad_census_an
triad_census_batagelj_mrvar <- function(
  bigraph
) {
  triad_census_batagelj_mrvar_C(
    el = as_edgelist(bigraph, names = FALSE)
  )
}

#' @rdname triad_census_an
triad_census_projection <- function(
  bigraph,
  verbose = FALSE
) {
  
  # Drop trivial cases
  if (vcount(bigraph) == 0) return(matrix(0, nrow = 0, ncol = 0))
  # Create projection
  graph <- actor_projection(bigraph, name = 'id')
  
  # Find maximum values of x and of w
  max.x <- max(E(graph)$weight)
  # Initialize matrix (overestimating the number of columns)
  C <- as.data.frame(matrix(0,
                            nrow = choose(max.x + 3, 3),
                            ncol = max.x + 1))
  
  # Tally one-tied triads
  ot <- oneTiedTriads(graph)
  # Insert the totals at the proper entries of C
  # (Aggregated, so no repeats, so no information loss)
  if (length(ot) > 0) C[sapply(ot$x, function(x) {
    partition_index(c(x, 0, 0))
  }) + 1, 1] <- ot$n
  if (verbose) print('One-tied triads tallied')
  
  # Tally two-tied triads
  tt <- twoTiedTriads(graph)
  # Insert the totals at the proper entries of C
  # (Aggregated, so no repeats, so no information loss)
  if (!is.null(tt)) C[sapply(1:dim(tt)[1], function(i) {
    partition_index(c(tt[i, 1], tt[i, 2], 0))
  }) + 1, 1] <- tt$n
  if (verbose) print('Two-tied triads tallied')
  
  # Tally triangles
  tht <- threeTiedTriads(bigraph, graph = graph)
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
  if (verbose) print('Three-tied triads tallied')
  
  # The remaining triads share no secondary nodes; count them as empty
  # (No triads should have yet been counted as empty)
  C[1, 1] <- choose(vcount(graph), 3) - sum(C)
  # Reality check: The total triad tally should equal |V(graph)|-choose-3
  # (but only makes sense within range of 'numeric' accuracy)
  # http://stackoverflow.com/questions/8804779/
  # what-is-integer-overflow-in-r-and-how-can-it-happen
  stopifnot(sum(C) == choose(vcount(graph), 3))
  # Clear names
  colnames(C) <- NULL
  as.matrix(C)
}

#' @rdname triad_census_an
#' @export
triad_census_difference <- function(
  bigraph,
  method = "batagelj_mrvar", ...,
  add.names = FALSE
) {
  
  # trivial case
  if (max(degree(bigraph, V(bigraph)$type)) <= 1) {
    tc <- matrix(0, nrow = 4, ncol = 2)
  } else {
    # method
    method <- match.arg(method, c("batagelj_mrvar", "projection"))
    triad_census_fun <- get(paste0("triad_census_difference_", method))
    tc <- triad_census_fun(bigraph = bigraph, ...)
  }
  
  # annotation
  if (add.names) {
    dimnames(tc) <- list(as.character(0:3), as.character(0:1))
  }
  
  tc
}

#' @rdname triad_census_an
triad_census_difference_batagelj_mrvar <- function(
  bigraph
) {
  triad_census_difference_batagelj_mrvar_C(
    el = as_edgelist(bigraph, names = FALSE)
  )
}

#' @rdname triad_census_an
triad_census_difference_projection <- function(
  bigraph
) {
  # Initialize the matrix and define the number of actors
  C <- matrix(0, nrow = 8, ncol = 2)
  n <- length(which(!V(bigraph)$type))
  # Trivial casess (not enough actors)
  if(n < 3) return(C)
  # Trivial case (no events)
  if((vcount(bigraph) - n) == 0) {
    C[1, 1] <- C[1, 1] + choose(n, 3)
    return(C)
  }
  
  # Create one-mode projection
  graph <- actor_projection(bigraph, name = 'id')
  # Leverage one-mode triad census for zero- or one-edged triads
  C[1:2, 1] <- simple_triad_census(graph)[1:2]
  if(sum(C) == choose(n, 3)) return(C)
  
  # Tally two-tied triads
  tt <- twoTiedTriads(graph)
  if(!is.null(tt)) {
    # Classify as 0,1,0 (equal edge wts) or 0,1,1 (distinct edge wts)
    ed <- stats::aggregate(tt$n, by = list(tt$x == tt$y), FUN = sum)
    # Insert the totals at the proper entries of C
    if(nrow(ed) == 1) C[4 - ed$Group.1[1], 1] <- ed$x[1] else
      C[3:4, 1] <- ed$x[2 - ed$Group.1]
  }
  
  # Find all triangles in the projection
  t <- do.call(cbind, cliques(graph, 3, 3))
  # Vector of triad weights
  w <- sapply(1:ncol(t), function(j) {
    shareWeight(bigraph, V(graph)$name[c(t[1, j], t[2, j], t[3, j])])
  })
  # Classify and tally
  l <- sapply(1:ncol(t), function(j) {
    # Pairwise exclusive counts
    pw <- sort(c(edgeWeight(graph, c(t[1, j], t[2, j])),
                 edgeWeight(graph, c(t[2, j], t[3, j])),
                 edgeWeight(graph, c(t[1, j], t[3, j])))) - w[j]
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

#' @rdname triad_census_an
#' @export
unif_triad_census <- function(bigraph) {
  .Deprecated("triad_census_difference")
  triad_census_difference(bigraph)
}

#' @rdname triad_census_an
#' @export
unif.triad.census <- function(bigraph) {
  .Deprecated("triad_census_difference")
  triad_census_difference(bigraph)
}

#' @rdname triad_census_an
#' @export
triad_census_binary <- function(
  bigraph,
  method = "batagelj_mrvar", ...,
  add.names = FALSE
) {
  
  # trivial case
  if (max(degree(bigraph, V(bigraph)$type)) <= 1) {
    tc <- matrix(0, nrow = 4, ncol = 2)
  } else {
    # method
    method <- match.arg(method, c("batagelj_mrvar", "projection"))
    triad_census_fun <- get(paste0("triad_census_binary_", method))
    tc <- triad_census_fun(bigraph = bigraph, ...)
  }
  
  # annotation
  if (add.names) {
    dimnames(tc) <- list(as.character(0:3), as.character(0:1))
  }
  
  tc
}

#' @rdname triad_census_an
triad_census_binary_batagelj_mrvar <- function(
  bigraph
) {
  triad_census_binary_batagelj_mrvar_C(
    el = as_edgelist(bigraph, names = FALSE)
  )
}

#' @rdname triad_census_an
triad_census_binary_projection <- function(
  bigraph,
  verbose = FALSE
) {
  # Initialize the matrix and define the number of actors
  C <- matrix(0, nrow = 4, ncol = 2)
  n <- length(which(!V(bigraph)$type))
  # Trivial casess (not enough actors)
  if(n < 3) return(C)
  # Trivial case (no events)
  if((vcount(bigraph) - n) == 0) {
    C[1, 1] <- C[1, 1] + choose(n, 3)
    return(C)
  }
  
  # Create one-mode projection
  graph <- actor_projection(bigraph, name = 'id')
  # Leverage one-mode triad census
  C[1:3, 1] <- simple_triad_census(graph)[1:3]
  if(sum(C) == choose(n, 3)) return(C)
  
  # Find all triangles in the projection
  t <- do.call(cbind, cliques(graph, 3, 3))
  # Vector of triad weights
  w <- sapply(1:ncol(t), function(j) {
    shareWeight(bigraph, V(graph)$name[c(t[1, j], t[2, j], t[3, j])])
  })
  w0 <- which(w == 0)
  C[4, 1] <- length(w0)
  
  # Restrict to triads with 3-actor events
  t <- t[, -w0]
  w <- w[-w0]
  # Compute the number of actor pairs with exclusive events in each
  l <- sapply(1:ncol(t), function(j) sum(c(
    edgeWeight(graph, c(t[1, j], t[2, j])),
    edgeWeight(graph, c(t[2, j], t[3, j])),
    edgeWeight(graph, c(t[1, j], t[3, j]))) > w[j]))
  C[, 2] <- tabulate(l + 1, nbins = 4)
  
  # Return the matrix
  stopifnot(sum(C) == choose(n, 3))
  C
}

#' @rdname triad_census_an
#' @export
str_triad_census <- function(bigraph) {
  .Deprecated("triad_census_binary")
  triad_census_binary(bigraph)
}

#' @rdname triad_census_an
#' @export
structural.triad.census <- function(bigraph) {
  .Deprecated("triad_census_binary")
  triad_census_binary(bigraph)
}
