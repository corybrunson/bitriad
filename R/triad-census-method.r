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
#' @return A matrix counts of triad congruence classes, with row indices 
#'   reflecting pairwise exclusive events and column indices reflecting 
#'   triadwise events.
#' @export
triad_census_method <- function(
  bigraph,
  method = "batagelj_mrvar", ...,
  add.names = FALSE
) {
  stopifnot(is_an(bigraph))
  method <- match.arg(method, c("batagelj_mrvar"))
  triad_census_fun <- get(paste0("triad_census_", method))
  tc <- triad_census_fun(bigraph = bigraph, ...)
  if (add.names) {
    colnames(tc) <- 0:(ncol(tc) - 1)
    rownames(tc) <- paste("(",
      sapply(0:(nrow(tc) - 1),
             function(i) paste(index_partition_C(i), collapse = ",")),
      ")", sep = "")
  }
  tc
}

triad_census_batagelj_mrvar <- function(
  bigraph
) {
  triad_census_batagelj_mrvar_C(
    el = as_edgelist(bigraph, names = FALSE),
    actors = as.numeric(V(bigraph)[V(bigraph)$type == FALSE]),
    max_weight = max(E(actor_projection(bigraph))$weight)
  )
}
