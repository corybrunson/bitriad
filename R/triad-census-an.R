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
#' @param census Character; the type of triad census to calculate, either
#'   \code{"full"} or \code{"binary"} (or \code{"structural"}).
#' @param method Character; the triad census method to use. Currently only 
#'   \code{"batagelj_mrvar"} is implemented. \code{"projection"} calls an 
#'   inefficient but reliable implementation in R from the first package version
#'   that invokes the \code{\link{simple_triad_census}} of the
#'   \code{\link{actor_projection}} of \code{bigraph}.
#' @param ... Additional arguments passed to the \code{method} function.
#' @param add.names Logical; whether to label the rows and columns of the output
#'   matrix.
#' @param verbose Logical; whether to display progress bars.
#' @param actors Numeric vector of \code{bigraph} actor node IDs (\strong{to 
#'   expedite the C++ function; to be obviated by a self-contained 
#'   implementation}).
#' @param max_weight Numeric; maximum number of events shared by two actors of 
#'   \code{bigraph} (\strong{to expedite the C++ function; to be obviated by a 
#'   self-contained implementation}).
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
  census = "full",
  method = "batagelj_mrvar", ...,
  add.names = FALSE
) {
  stopifnot(is_an(bigraph))
  
  # type of census
  census <- match.arg(census, c("full", "binary", "structural"))
  if (census == "full") {
    return(triad_census_full(bigraph = bigraph,
                             method = method, ...,
                             add.names = add.names))
  } else {
    return(triad_census_binary(bigraph = bigraph,
                               method = method, ...,
                               add.names = add.names))
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
#' @export
triad_census_batagelj_mrvar <- function(
  bigraph
) {
  triad_census_batagelj_mrvar_C(
    el = as_edgelist(bigraph, names = FALSE)
  )
}

#' @rdname triad_census_an
#' @export
triad_census_batagelj_mrvar_alt <- function(
  bigraph,
  actors = NULL, max_weight = NULL
) {
  if (is.null(actors)) {
    actors <- as.numeric(V(bigraph)[V(bigraph)$type == FALSE])
  }
  if (is.null(max_weight)) {
    max_weight <- max(E(actor_projection(bigraph))$weight)
  }
  triad_census_batagelj_mrvar_alt_C(
    el = as_edgelist(bigraph, names = FALSE),
    actors = actors,
    max_weight = max_weight
  )
}

#' @rdname triad_census_an
#' @export
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
#' @export
triad_census_binary_batagelj_mrvar <- function(
  bigraph
) {
  triad_census_binary_batagelj_mrvar_C(
    el = as_edgelist(bigraph, names = FALSE)
  )
}

#' @rdname triad_census_an
#' @export
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
str_triad_census <- triad_census_binary

#' @rdname triad_census_an
#' @export
structural.triad.census <- triad_census_binary
