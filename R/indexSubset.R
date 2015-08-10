#' Combinatorial bijections for affiliation network triad indexing
#' 
#' These functions biject among partitions of at most 3 parts, 3-subsets of
#' natural numbers, and indices for the lexicographic total orders on them.
#' @param i An index in the total order; starts at 0.
#' @export
#' @examples
#' indexSubset(2)
#' @return A (decreasing) 3-subset of natural numbers (including 0).

indexSubset <-
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
