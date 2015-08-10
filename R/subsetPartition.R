#' Combinatorial bijections for affiliation network triad indexing
#' 
#' These functions biject among partitions of at most 3 parts, 3-subsets of
#' natural numbers, and indices for the lexicographic total orders on them.
#' @param vec A (decreasing) 3-subset of natural numbers (including 0).
#' @export
#' @examples
#' subsetPartition(c(3, 2, 0))
#' @return A partition of 3 parts (possibly including 0).

subsetPartition <-
    function(vec) vec - (length(vec) - 1):0
