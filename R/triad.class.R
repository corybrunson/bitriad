#' Triad classification
#' 
#' Given three actor nodes in an affiliation network, determine the isomorphism
#' class of the two-mode triad they form by its index (a partition of three
#' parts and a nonnegative integer).
#' @param bigraph An affiliation network
#' @param v A subset of three actor nodes in bigraph
#' @export

triad.class <-
function(bigraph, v) {
    stopifnot(all(V(bigraph)$type[v] == 0) & length(v) == 3)
    pairs <- neighborhood(bigraph, 1, v)
    w <- length(which(table(unlist(pairs)) > 2))
    lambda <- sort(sapply(pairs, length) - 1 - w, decreasing = TRUE)
    return(list(lambda = lambda, w = w))
}
