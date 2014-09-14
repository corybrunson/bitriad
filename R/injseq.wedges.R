#' Wedges (injective morphisms modulo structural equivalence)
#' 
#' Each clustering coefficient can be defined as the proportion of "wedges" that
#' are "closed", for suitable definitions of both terms. These functions count
#' the "wedges", and among them the "closed" ones, centered at a given actor
#' node in a given affiliation network.
#' @param bigraph The ambient affiliation network.
#' @param Q An actor node in the network.
#' @export

injseq.wedges <-
function(bigraph, Q) {
    # Identify nodes of separation (exactly) 1 and 2 from Q
    n1 <- setdiff(neighborhood(bigraph, 1, Q)[[1]], Q)
    n2 <- setdiff(neighborhood(bigraph, 2, Q)[[1]], c(n1, Q))
    # Identify events attended by these actors
    n2n1 <- lapply(neighborhood(bigraph, 1, n2), setdiff, n2)

    # Require at least two nodes of separation 2 for a wedge
    if(length(n2) < 2) return(c(0, 0))
    # Identify pairs (P, R) of nodes in n2
    p <- combn(length(n2), 2)
    
    # Count the structurally distinct wedges (0 thru 4) for each pair (P, R)
    # and the number of these that are closed
    wedgelist <- sapply(1:ncol(p), function(j) {
        # Whether P and Q share an exclusive event
        pq <- length(setdiff(intersect(n1, n2n1[[p[1, j]]]),
                             n2n1[[p[2, j]]])) > 0
        # Whether Q and R share an exclusive event
        qr <- length(setdiff(intersect(n1, n2n1[[p[2, j]]]),
                             n2n1[[p[1, j]]])) > 0
        # How many events P, Q, and R share
        pqr <- length(intersect(intersect(n1, n2n1[[p[1, j]]]),
                                n2n1[[p[2, j]]]))
        # If revelant, whether P and R share an exclusive event
        pr <- if(pq + qr + pqr < 2) 0 else
            (length(setdiff(intersect(n2n1[[p[1, j]]], n2n1[[p[2, j]]]), n1)) >
                 0)
        # Counts
        Ws <- c(pq * qr, c(pq, qr) * (pqr > 0), pqr > 1)
        Ts <- c(pr + pqr > 0, rep(pr | (pqr > 1), 2), pr | (pqr > 2))
        return(c(sum(Ws), sum(Ws * Ts)))
    })
    return(rowSums(wedgelist))
}
