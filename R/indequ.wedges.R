#' Affiliation network wedges
#' 
#' Each clustering coefficient can be defined as the proportion of "wedges" that
#' are "closed", for suitable definitions of both terms. These functions count
#' the "wedges", and among them the "closed" ones, centered at a given actor
#' node in a given affiliation network.
#' @param bigraph The ambient affiliation network.
#' @param Q An actor node in the network.
#' @family wedge functions
#' @export

indequ.wedges <-
    function(bigraph, Q) {
        # Identify secondary neighbors of Q
        n1 <- setdiff(neighborhood(bigraph, 1, Q)[[1]], Q)
        # If there aren't at least two, return zeroes
        if(length(n1) < 2) return(c(0, 0))
        # Identify primary neighborhoods of secondary neighbors of Q
        n1n1 <- lapply(neighborhood(bigraph, 1, n1), setdiff, c(Q, n1))
        # Array the 2-paths centered at Q
        # (Note that these are indices of n1, not vertex ids)
        p <- combn(1:length(n1), 2)
        # Across the pairs (X, Y) list the numbers of wedges and closed wedges
        wedgelist <- do.call(cbind, lapply(1:ncol(p), function(j) {
            # The first node X must have a nonempty neighborhood besides Q
            # and at least one neighbor not tied to Y (= n1[p[2, j]])
            Ps <- setdiff(n1n1[[p[1, j]]], n1n1[[p[2, j]]])
            if(length(Ps) == 0) return(c(0, 0))
            # Across all choices of P from the non-Q primary neighbors of X
            # that are not tied to Y
            do.call(cbind, lapply(Ps, function(P) {
                # Y must have a nonempty nbhd besides Q and P,
                # from which R, which cannot be tied to X, is to be drawn
                Rs <- setdiff(n1n1[[p[2, j]]], c(P, n1n1[[p[1, j]]]))
                if(length(Rs) == 0) return(c(0, 0))
                # Which Rs produce 4-paths (P, X, Q, Y, R) that are closed
                # by events Z that are not tied to Q?
                Rw <- which(sapply(Rs, function(R) {
                    length(setdiff(
                        intersect(neighborhood(bigraph, 1, P)[[1]],
                                  neighborhood(bigraph, 1, R)[[1]]),
                        n1)) > 0
                }))
                return(c(length(Rs), length(Rw)))
            }))
        }))
        rowSums(wedgelist)
    }
