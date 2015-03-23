#' Combinatorial bijections for affiliation network triad labeling
#' 
#' These functions biject among partitions of at most 3 parts, 3-subsets of
#' natural numbers, and indices for the lexicographic total orders on both.
#' @param vec A (decreasing) 3-subset of natural numbers (including 0).
#' @export
#' @examples
#' subsetPartition(c(3, 2, 0))

subsetPartition <-
    function(vec) vec - (length(vec) - 1):0
