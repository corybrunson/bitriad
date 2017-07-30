#' @title Triad census for affiliation networks
#'   
#' @description Given an affiliation network, tally all actor triads by 
#'   isomorphism class.
#'   
#' @details The \code{triad_census_*} functions implement the four triad
#'   censuses described below in matrix form.
#'   
#' @template triadcensus
#' 

#' @name triad_census_an
#' @param bigraph An affiliation network.
#' @param method Character; the triad census method to use. Currently only 
#'   \code{"batagelj_mrvar"} is implemented. \code{"original"} calls an 
#'   inefficient but reliable original implementation in R.
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
  method = "batagelj_mrvar", ...,
  add.names = FALSE
) {
  stopifnot(is_an(bigraph))
  method <- match.arg(method, c("batagelj_mrvar", "original"))
  triad_census_fun <- get(paste0("triad_census_", method))
  tc <- triad_census_fun(bigraph = bigraph, ...)
  if (add.names) {
    colnames(tc) <- 0:(ncol(tc) - 1)
    rownames(tc) <- paste(
      "(",
      sapply(0:(nrow(tc) - 1),
             function(i) paste(index_partition_C(i), collapse = ",")),
      ")", sep = ""
    )
  }
  tc
}

#' @rdname triad_census_an
#' @export
triad.census.an <- triad_census_an

#' @rdname triad_census_an
#' @export
triad_census_batagelj_mrvar <- function(
  bigraph,
  actors = NULL, max_weight = NULL
) {
  if (is.null(actors)) {
    actors <- as.numeric(V(bigraph)[V(bigraph)$type == FALSE])
  }
  if (is.null(max_weight)) {
    max_weight <- max(E(actor_projection(bigraph))$weight)
  }
  triad_census_batagelj_mrvar_C(
    el = as_edgelist(bigraph, names = FALSE),
    actors = actors,
    max_weight = max_weight
  )
}

#' @rdname triad_census_an
#' @export
triad_census_original <- function(
  bigraph,
  verbose = FALSE
) {
  
  # Drop trivial cases
  if (vcount(bigraph) == 0) return(matrix(0, nrow = 0, ncol = 0))
  # Create projection
  graph <- actor_projection(bigraph, name = 'id')
  # Trivial case
  if (ecount(graph) == 0) {
    C <- matrix(choose(vcount(graph), 3), nrow = 1, ncol = 1)
    return(C)
  }
  
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
