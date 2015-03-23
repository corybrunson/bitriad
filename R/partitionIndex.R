#' Combinatorial bijections for affiliation network triad labeling
#' 
#' These functions biject among partitions of at most 3 parts, 3-subsets of
#' natural numbers, and indices for the lexicographic total orders on both.
#' @param lambda A partition of at most 3 parts.
#' @export
#' @examples
#' partitionIndex(c(1, 1, 0))

partitionIndex <-
    function(lambda) {
        subsetIndex(partitionSubset(lambda))
    }
