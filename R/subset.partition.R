#' Combinatorial bijections for affiliation triad labeling
#' 
#' These functions biject among partitions of at most 3 parts, 3-subsets of
#' natural numbers, and indices for the lexicographic total orders on both.
#' @param vec A (decreasing) 3-subset of natural numbers (including 0).
#' @export
#' @examples
#' subset.partition(c(3, 2, 0))

subset.partition <-
function(vec) vec - (length(vec) - 1):0
