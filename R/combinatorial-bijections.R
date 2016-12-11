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
#' index_subset(2)
#' index_partition(2)
#' subset_index(c(3, 2, 0))
#' subset_partition(c(3, 2, 0))
#' partition_index(c(1, 1, 0))
#' partition_subset(c(1, 1, 0))
#' @export
index_subset <-
    function(i) {
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
index_partition <-
    function(i) {
        subset_partition(index_subset(i))
    }

#' @rdname combinatorial_bijections
#' @export
subset_index <-
    function(vec) {
        stopifnot(!is.unsorted(rev(vec), strictly = TRUE))
        sum(choose(rev(vec), 1:length(vec)))
    }

#' @rdname combinatorial_bijections
#' @export
subset_partition <-
    function(vec) vec - (length(vec) - 1):0

#' @rdname combinatorial_bijections
#' @export
partition_index <-
    function(lambda) {
        subset_index(partition_subset(lambda))
    }

#' @rdname combinatorial_bijections
#' @export
partition_subset <-
    function(lambda) lambda + (length(lambda) - 1):0

#' @rdname combinatorial_bijections
#' @export
indexSubset <- index_subset

#' @rdname combinatorial_bijections
#' @export
indexPartition <- index_partition

#' @rdname combinatorial_bijections
#' @export
subsetIndex <- subset_index

#' @rdname combinatorial_bijections
#' @export
subsetPartition <- subset_partition

#' @rdname combinatorial_bijections
#' @export
partitionIndex <- partition_index

#' @rdname combinatorial_bijections
#' @export
partitionSubset <- partition_subset
