#' Combinatorial bijections for affiliation network triad indexing
#' 
#' These functions biject among partitions of at most 3 parts, 3-subsets of
#' natural numbers, and indices for the lexicographic total orders on them.
#' @param lambda A partition of 3 parts (possibly including 0).
#' @export
#' @examples
#' partitionSubset(c(1, 1, 0))
#' @return A (decreasing) 3-subset of natural numbers (including 0).

partitionSubset <-
    function(lambda) lambda + (length(lambda) - 1):0
