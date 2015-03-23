#' Combinatorial bijections for affiliation network triad labeling
#' 
#' These functions biject among partitions of at most 3 parts, 3-subsets of
#' natural numbers, and indices for the lexicographic total orders on both.
#' @param vec A (decreasing) 3-subset of natural numbers (including 0).
#' @export
#' @examples
#' subsetIndex(c(3, 2, 0))

subsetIndex <-
    function(vec) {
        stopifnot(!is.unsorted(rev(vec), strictly = TRUE))
        sum(choose(rev(vec), 1:length(vec)))
    }
