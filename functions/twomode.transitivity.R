# Hall's criterion for the existence of a system of distinct representatives
hall.criterion <- function(lst) all(sapply(0:(2 ^ length(lst) - 1),
                                           function(i) {
    w <- which(intToBits(i) == 1)
    length(unique(unlist(lst[w]))) >= length(w)
}))

# Wedges function for Opsahl's clustering coefficient
opsahl.wedges <- function(bigraph, P, Q, R, n1) {
    nP <- neighborhood(bigraph, 1, P)[[1]]
    nR <- neighborhood(bigraph, 1, R)[[1]]
    nPQ <- intersect(n1, nP)
    nQR <- intersect(n1, nR)
    nPR <- intersect(nP, nR)
    wedgelist <- rbind(rep(nPQ, each = length(nQR)),
                       rep(nQR, times = length(nPQ)))
    wedgelist <- wedgelist[, wedgelist[1, ] != wedgelist[2, ]]
    if(length(nPR) > 2) return(rep(dim(wedgelist)[2], 2))
    are.closed <- sapply(1:dim(wedgelist)[2], function(j) {
        length(setdiff(nPR, wedgelist[, j])) > 0
    })
    return(c(length(are.closed), sum(are.closed)))
}

# Two-mode clustering coefficient shell
# Opsahl, inclusive, and exclusive clustering coefficients as specializations
twomode.transitivity <- function(
    bigraph, node.type = 0, type = 'global', wedges.fn = opsahl.wedges,
    vids = which(V(bigraph)$type == node.type), verbose = FALSE
    ) {
    # Check that nodes are of the desired type
    stopifnot(all(V(bigraph)$type[vids] == node.type))
    # If global or both, need to look at all vertices
    Qs <- if(type != 'local') which(V(bigraph)$type == node.type) else vids
    # Progress bar runs over vs
    if(verbose) {
        pb <- txtProgressBar(min = 0, max = length(vs), style = 3)
        step <- 0
    }
    # Array of 4-paths centered at each v in vs
    wedges <- matrix(unlist(lapply(Qs, function(Q) {
        # Update progress bar
        if(verbose) {
            step <- step + 1
            setTxtProgressBar(pb, step)
        }
        # Identify the nodes at distances 1 and 2 from Q
        n1 <- setdiff(neighborhood(bigraph, 1, Q)[[1]], Q)
        n2 <- setdiff(neighborhood(bigraph, 2, Q)[[1]], c(Q, n1))
        # If fewer than two nodes are distance 2 from Q then no wedges exist
        if(length(n2) < 2) return(c(V = 0, C = 0))
        # Return wedge and closed wedge counts
        wc <- do.call(cbind, sapply(1:(length(n2) - 1), function(i) {
            sapply((i + 1):length(n2), function(j) {
                wedges.fn(bigraph, n2[i], Q, n2[j], n1)
            })
        }))
        return(rowSums(wc))
    })), nr = 2)
    # Close progress bar
    if(verbose) close(pb)
    if(type == 'local') return(wedges[1, ] / wedges[2, ])
    if(type == 'global') return(sum(wedges[1, ]) / sum(wedges[2, ]))
    return(wedges)
}
