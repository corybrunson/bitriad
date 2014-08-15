#' Combinatorial bijections for affiliation triad labeling
#' 
#' These functions biject among partitions of at most 3 parts, 3-subsets of
#' natural numbers, and indices for the lexicographic total orders on both.
#' @param i An index under the total order. Starts at 0.
#' @param vec A (decreasing) 3-subset of natural numbers (including 0).
#' @param lambda A partition of at most 3 parts.
#' @export
#' @examples
#' subset.index(c(3, 2, 0))

subset.index <-
function(vec) {
  stopifnot(!is.unsorted(rev(vec), strictly = TRUE))
  sum(choose(rev(vec), 1:length(vec)))
}
