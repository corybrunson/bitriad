#' Combinatorial bijections for affiliation network triad indexing
#' 
#' These functions biject among partitions of at most 3 parts, 3-subsets of
#' natural numbers, and indices for the lexicographic total orders on them.
#' @param lambda A partition of 3 parts (possibly including 0).
#' @export
#' @examples
#' partitionIndex(c(1, 1, 0))
#' @return An index in the total order; starts at 0.

partitionIndex <-
    function(lambda) {
        subsetIndex(partitionSubset(lambda))
    }
