#' Combinatorial bijections for affiliation network triad indexing
#' 
#' These functions biject among partitions of at most 3 parts, 3-subsets of
#' natural numbers, and indices for the lexicographic total orders on them.
#' 
#' @name combinatorial_bijections
#' @param i An index in the total order; starts at 0.
#' @param vec A (decreasing) 3-subset of natural numbers (including 0).
#' @param lambda A partition of at most 3 parts.
#' @examples
#' index_subset_R(2)
#' index_partition_R(2)
#' subset_index_R(c(3, 2, 0))
#' subset_partition_R(c(3, 2, 0))
#' partition_index_R(c(1, 1, 0))
#' partition_subset_R(c(1, 1, 0))
NULL

#' @rdname combinatorial_bijections
#' @export
index_subset_R <- function(i) {
  vec <- c()
  N <- i
  for(j in 3:1) {
    c <- j - 1
    while(choose(c + 1, j) <= N) c <- c + 1
    vec <- c(vec, c)
    N <- N - choose(c, j)
  }
  return(vec)
}

#' @rdname combinatorial_bijections
#' @export
subset_index_R <- function(vec) {
  stopifnot(!is.unsorted(rev(vec), strictly = TRUE))
  sum(choose(rev(vec), 1:length(vec)))
}

#' @rdname combinatorial_bijections
#' @export
subset_partition_R <- function(vec) vec - (length(vec) - 1):0

#' @rdname combinatorial_bijections
#' @export
partition_subset_R <- function(lambda) lambda + (length(lambda) - 1):0

#' @rdname combinatorial_bijections
#' @export
index_partition_R <- function(i) {
  subset_partition_R(index_subset_R(i))
}

#' @rdname combinatorial_bijections
#' @export
partition_index_R <- function(lambda) {
  subset_index_R(partition_subset_R(lambda))
}

#' @rdname combinatorial_bijections
#' @export
indexSubset <- index_subset_R

#' @rdname combinatorial_bijections
#' @export
indexPartition <- index_partition_R

#' @rdname combinatorial_bijections
#' @export
subsetIndex <- subset_index_R

#' @rdname combinatorial_bijections
#' @export
subsetPartition <- subset_partition_R

#' @rdname combinatorial_bijections
#' @export
partitionIndex <- partition_index_R

#' @rdname combinatorial_bijections
#' @export
partitionSubset <- partition_subset_R
