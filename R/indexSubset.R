#' Combinatorial bijections for affiliation network triad labeling
#' 
#' These functions biject among partitions of at most 3 parts, 3-subsets of
#' natural numbers, and indices for the lexicographic total orders on both.
#' @param i An index under the total order. Starts at 0.
#' @export
#' @examples
#' indexSubset(2)

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
