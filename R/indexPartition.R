#' Combinatorial bijections for affiliation network triad labeling
#' 
#' These functions biject among partitions of at most 3 parts, 3-subsets of
#' natural numbers, and indices for the lexicographic total orders on both.
#' @param i An index under the total order. Starts at 0.
#' @export
#' @examples
#' indexPartition(2)

indexPartition <-
    function(i) {
        subsetPartition(indexSubset(i))
    }
