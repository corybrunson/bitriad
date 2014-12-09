#' Wedges
#' 
#' Each clustering coefficient can be defined as the proportion of "wedges" that
#' are "closed", for suitable definitions of both terms. These functions count
#' the "wedges", and among them the "closed" ones, centered at a given actor
#' node in a given affiliation network.
#' @param bigraph The ambient affiliation network.
#' @param Q An actor node in the network.

indstr.wedges <-
    function(bigraph, Q) {
        # Identify nodes of separation (exactly) 1 and 2 from Q
        n1 <- setdiff(neighborhood(bigraph, 1, Q)[[1]], Q)
        n2 <- setdiff(neighborhood(bigraph, 2, Q)[[1]], c(n1, Q)) # rm Q?
        # Require at least two nodes of separation 2 for a wedge
        if(length(n2) < 2) return(c(0, 0))
        # Identify secondary neighbors of primary neighbors P of Q (excluding P)
        n2n1 <- lapply(n2,
                       function(P) setdiff(neighborhood(bigraph, 1, P)[[1]], P))
        # Identify indexes of pairs (P, R) of nodes in n2
        p <- combn(1:length(n2), 2)
        # Remove pairs (P, R) with no pairwise exclusive secondary neighbors
        p <- as.matrix(p[, sapply(1:dim(p)[2], function(j) {
            (0 < length(setdiff(intersect(n2n1[[p[1, j]]], n1),
                                n2n1[[p[2, j]]])) *
                 length(setdiff(intersect(n2n1[[p[2, j]]], n1),
                                n2n1[[p[1, j]]])))
        })], nr = 2)
        # Require at least two nodes of separation 2 for a wedge
        if(dim(p)[2] == 0) return(c(0, 0))
        # Identify which of these pairs share a neighbor not shared with Q
        cl <- sapply(1:dim(p)[2], function(j) {
            0 < length(setdiff(intersect(n2n1[[p[1, j]]], n2n1[[p[2, j]]]), n1))
        })
        # Return the counts
        return(c(length(cl), sum(cl)))
    }
