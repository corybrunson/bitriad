#' Structural triad census
#' 
#' This function computes the structural triad census for an affiliation
#' network, a 4-by-2 array of frequency counts for each isomorphism class of
#' triad, modulo an equivalence relation of event nodes by which actors they
#' are tied to, which sum to choose(n, 3), where n is the number of actor nodes.
#' @param bigraph The affiliation network
#' @param type The actor node type in bigraph
#' @param verbose Logical; whether to display progress bars
#' @export

str.triad.census <-
    function(bigraph, type = 0, verbose = FALSE) {
        # Initialize the matrix and define the number of actors
        C <- matrix(0, nr = 4, nc = 2)
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
        # Leverage one-mode triad census
        C[1:3, 1] <- simple.triad.census(graph)[1:3]
        if(sum(C) == choose(n, 3)) return(C)
        
        # Find all triangles in the projection
        t <- do.call(cbind, cliques(graph, 3, 3))
        # Vector of triad weights
        w <- sapply(1:ncol(t), function(j) {
            share.weight(bigraph, V(graph)$name[c(t[1, j], t[2, j], t[3, j])])
        })
        w0 <- which(w == 0)
        C[4, 1] <- length(w0)
        
        # Restrict to triads with 3-actor events
        t <- t[, -w0]
        w <- w[-w0]
        # Compute the number of actor pairs with exclusive events in each
        l <- sapply(1:ncol(t), function(j) sum(c(
            edge.weight(graph, c(t[1, j], t[2, j])),
            edge.weight(graph, c(t[2, j], t[3, j])),
            edge.weight(graph, c(t[1, j], t[3, j]))) > w[j]))
        C[, 2] <- tabulate(l + 1, nbins = 4)
        
        # Return the matrix
        stopifnot(sum(C) == choose(n, 3))
        C
    }
