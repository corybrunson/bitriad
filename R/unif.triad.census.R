#' Uniformity triad census
#' 
#' This function computes the uniformity triad census for an affiliation
#' network, an 8-by-2 array of frequency counts for each isomorphism class of
#' triad, modulo the equivalence relation that identifies triads based on the
#' positivity of their triad weights w and of the differences between their
#' sequential pairwise weights. This census can be recovered from the full triad
#' census, and the structural equivalence triad census can be recovered from it.
#' The census must sum to choose(n, 3), where n is
#' the number of actor nodes.
#' @param bigraph The affiliation network
#' @param type The actor node type in bigraph
#' @export

unif.triad.census <- function(bigraph, type = 0) {
    # Initialize the matrix and define the number of actors
    C <- matrix(0, nr = 8, nc = 2)
    n <- length(which(V(bigraph)$type == type))
    # Trivial casess (not enough actors)
    if(n < 3) return(C)
    # Trivial case (no events)
    if((vcount(bigraph) - n) == 0) {
        C[1, 1] <- C[1, 1] + choose(n, 3)
        return(C)
    }
    
    # Create one-mode projection
    graph <- actor.projection(bigraph, type = type, name = 'id')
    # Leverage one-mode triad census for zero- or one-edged triads
    C[1:2, 1] <- simple.triad.census(graph)[1:2]
    if(sum(C) == choose(n, 3)) return(C)
    
    # Tally two-tied triads
    tt <- two.tied.triads(graph)
    if(!is.null(tt)) {
        # Classify as 0,1,0 (equal edge wts) or 0,1,1 (distinct edge wts)
        ed <- aggregate(tt$n, by = list(tt$x == tt$y), FUN = sum)
        # Insert the totals at the proper entries of C
        if(nrow(ed) == 1) C[4 - ed$Group.1[1], 1] <- ed$x[1] else
            C[3:4, 1] <- ed$x[2 - ed$Group.1]
    }
    
    # Find all triangles in the projection
    t <- do.call(cbind, cliques(graph, 3, 3))
    # Vector of triad weights
    w <- sapply(1:ncol(t), function(j) {
        share.weight(bigraph, V(graph)$name[c(t[1, j], t[2, j], t[3, j])])
    })
    # Classify and tally
    l <- sapply(1:ncol(t), function(j) {
        # Pairwise exclusive counts
        pw <- sort(c(edge.weight(graph, c(t[1, j], t[2, j])),
                     edge.weight(graph, c(t[2, j], t[3, j])),
                     edge.weight(graph, c(t[1, j], t[3, j])))) - w[j]
        # Equal or distinct pairwise exclusive event counts
        ed <- c(0, pw[1:2]) < pw
        # Row index in C
        sum((2 ^ (2:0)) * ed)
    })
    # Store tallies in C
    C[5:8, 1] <- tabulate(l[w == 0] + 1, nbins = 8)[5:8]
    C[, 2] <- tabulate(l[w > 0] + 1, nbins = 8)
    
    # Return the matrix
    stopifnot(sum(C) == choose(n, 3))
    C
}
