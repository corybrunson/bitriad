#' Wedges
#' 
#' Each clustering coefficient can be defined as the proportion of "wedges" that
#' are "closed", for suitable definitions of both terms. These functions count
#' the "wedges", and among them the "closed" ones, centered at a given actor
#' node in a given affiliation network.
#' @param bigraph The ambient affiliation network.
#' @param Q An actor node in the network.
#' @export

injact.wedges <-
function(bigraph, Q) {
        # Identify nodes of separation (exactly) 1 and 2 from Q
        n1 <- setdiff(neighborhood(bigraph, 1, Q)[[1]], Q)
        n2 <- setdiff(neighborhood(bigraph, 2, Q)[[1]], c(n1, Q))
        # Require at least two nodes of separation 2 for a wedge
        if(length(n2) < 2) return(c(0, 0))
        # Identify pairs (P, R) of nodes in n2
        p <- combn(n2, 2)
        # Identify which of these pairs form wedges & which of these are closed
        wedgelist <- sapply(1:dim(p)[2], function(j) {
            # Secondary neighbors of P and of R
            pn1 <- neighborhood(bigraph, 1, p[1:2, j])
            # Common neighbors of P and R
            tn1 <- do.call(intersect, pn1)
            # If only one secondary links either to Q then no wedges exist
            if(length(intersect(n1, unique(unlist(pn1)))) == 1) c(0, 0) else
                # Otherwise one wedge, closed iff Hall criterion is met
                c(1, as.numeric(hallCriterion(list(intersect(n1, pn1[[1]]),
                                                    intersect(n1, pn1[[2]]),
                                                    tn1))))
        })
        return(rowSums(wedgelist))
    }
