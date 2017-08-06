#' @title Combinatorial bijections for affiliation network triad indexing
#'   
#' @description These functions biject among partitions of at most 3 parts, 
#'   3-subsets of natural numbers, and indices for the lexicographic total 
#'   orders on them.
#'   
#' @name combinatorial-bijections
#' @param i Integer; an index in the total order, starting at 0.
#' @param vec Integer vector; a set of 3 distinct non-negative integers, in 
#'   decreasing order.
#' @param lambda Integer vector; a partition of at most 3 parts, with parts in 
#'   non-increasing order.
#' @examples
#' index_subset(2)
#' index_partition(2)
#' subset_index(c(3, 2, 0))
#' subset_partition(c(3, 2, 0))
#' partition_index(c(1, 1, 0))
#' partition_subset(c(1, 1, 0))
NULL

#' @rdname combinatorial-bijections
#' @export
indexSubset <- function(i) {
  .Deprecated("index_subset")
  index_subset(i)
}

#' @rdname combinatorial-bijections
#' @export
indexPartition <- function(i) {
  .Deprecated("index_partition")
  index_partition(i)
}

#' @rdname combinatorial-bijections
#' @export
subsetIndex <- function(vec) {
  .Deprecated("subset_index")
  subset_index(vec)
}

#' @rdname combinatorial-bijections
#' @export
subsetPartition <- function(vec) {
  .Deprecated("subset_partition")
  subset_partition(vec)
}

#' @rdname combinatorial-bijections
#' @export
partitionIndex <- function(lambda) {
  .Deprecated("partition_index")
  partition_index(lambda)
}

#' @rdname combinatorial-bijections
#' @export
partitionSubset <- function(lambda) {
  .Deprecated("partition_subset")
  partition_subset(lambda)
}
