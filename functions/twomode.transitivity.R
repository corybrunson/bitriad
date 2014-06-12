# Hall's criterion for the existence of a system of distinct representatives
hall.criterion <- function(lst) all(sapply(0:(2 ^ length(lst) - 1),
                                           function(i) {
    w <- which(intToBits(i) == 1)
    length(unique(unlist(lst[w]))) >= length(w)
}))

# Wedges through a given node in a given two-mode network (classical)
classical.wedges <- function(bigraph, Q) {
    
}

# Wedges through a given node in a given two-mode network (Opsahl)
# Agrees globally and locally with bipartite.transitivity on four small networks
opsahl.wedges <- function(bigraph, Q) {
    # Identify secondary neighbors of Q
    n1 <- setdiff(neighborhood(bigraph, 1, Q)[[1]], Q)
    # If there aren't at least two, return zeroes
    if(length(n1) < 2) return(c(0, 0))
    # Identify primary neighborhoods of secondary neighbors of Q
    n1n1 <- lapply(neighborhood(bigraph, 1, n1), setdiff, c(Q, n1))
    # Array the 2-paths centered at Q
    # (Note that these are indices of n1n1, not vertex ids)
    p <- combn(1:length(n1), 2)
    # Across the pairs (X, Y) list the numbers of wedges and of closed wedges
    wedgelist <- do.call(cbind, lapply(1:dim(p)[2], function(j) {
        # The first node X must have a nonempty neighborhood besides Q
        if(length(n1n1[[p[1, j]]]) == 0) return(c(0, 0))
        # Across all choices of P from the non-Q primary neighbors of X
        do.call(cbind, lapply(n1n1[[p[1, j]]], function(P) {
            # The second node Y must have a nonempty nbhd besides Q and P
            Rs <- setdiff(n1n1[[p[2, j]]], P)
            if(length(Rs) == 0) return(c(0, 0))
            # Which Rs produce 4-paths (P, X, Q, Y, R) that are closed?
            Rw <- which(sapply(Rs, function(R) {
                length(setdiff(intersect(neighborhood(bigraph, 1, P)[[1]],
                                         neighborhood(bigraph, 1, R)[[1]]),
                               n1[p[, j]])) > 0
            }))
            return(c(length(Rs), length(Rw)))
        }))
    }))
    return(rowSums(wedgelist))
}

# Wedges through a given node in a given two-mode network (inclusive)
incl.wedges <- function(bigraph, Q) {
    # Identify nodes of separation (exactly) 1 and 2 from v
    n1 <- setdiff(neighborhood(bigraph, 1, Q)[[1]], Q)
    n2 <- setdiff(neighborhood(bigraph, 2, Q)[[1]], c(n1, Q))
    # Require at least two nodes of separation 2 for a wedge
    if(length(n2) < 2) return(c(0, 0))
    # Identify pairs (P, R) of nodes in n2
    p <- combn(n2, 2)
    # Identify which of these pairs form wedges and, of these, which are closed
    wedgelist <- sapply(1:dim(p)[2], function(j) {
        # Secondary neighbors of P and of R
        pn1 <- neighborhood(bigraph, 1, p[1:2, j])
        # Common neighbors of P and R
        tn1 <- do.call(intersect, pn1)
        # If only one secondary links either to Q then no wedges exist
        if(length(intersect(n1, unique(unlist(pn1)))) == 1) c(0, 0) else
            # Otherwise one wedge, closed iff Hall criterion is met
            c(1, as.numeric(hall.criterion(list(intersect(n1, pn1[[1]]),
                                                intersect(n1, pn1[[2]]),
                                                tn1))))
    })
    return(rowSums(wedgelist))
}

# Wedges through a given node in a given two-mode network (exclusive)
excl.wedges <- function(bigraph, Q) {
    # Identify nodes of separation (exactly) 1 and 2 from v
    n1 <- setdiff(neighborhood(bigraph, 1, Q)[[1]], Q)
    n2 <- setdiff(neighborhood(bigraph, 2, Q)[[1]], c(n1, Q)) # rm Q?
    # Require at least two nodes of separation 2 for a wedge
    if(length(n2) < 2) return(c(0, 0))
    # Identify secondary neighbors of primary neighbors P of Q (excluding P)
    n2n1 <- lapply(n2, function(P) setdiff(neighborhood(bigraph, 1, P)[[1]], P))
    # Identify indexes of pairs (P, R) of nodes in n2
    p <- combn(1:length(n2), 2)
    # Remove pairs (P, R) that have no pairwise exclusive secondary neighbors
    p <- as.matrix(p[, sapply(1:dim(p)[2], function(j) {
        (0 < length(setdiff(intersect(n2n1[[p[1, j]]], n1), n2n1[[p[2, j]]])) *
             length(setdiff(intersect(n2n1[[p[2, j]]], n1), n2n1[[p[1, j]]])))
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

# Generic two-mode clustering coefficient
# (Progress bars don't work in apply functions; try pbapply package)
twomode.transitivity <- function(
    bigraph, node.type = 0, type = 'global', wedges.fn = opsahl.wedges,
    vids = which(V(bigraph)$type == node.type)
    ) {
    # Check that nodes are of the desired type
    stopifnot(all(V(bigraph)$type[vids] == node.type))
    # If global or both, need to look at all vertices
    Qs <- if(type != 'local') which(V(bigraph)$type == node.type) else vids
    # Array of 4-paths centered at each Q in Qs
    wedges <- matrix(unlist(lapply(Qs, function(Q) {
        # Return wedge and closed wedge counts at Q
        return(wedges.fn(bigraph, Q))
    })), nr = 2)
    if(type == 'global') return(sum(wedges[2, ]) / sum(wedges[1, ]))
    if(type == 'local') return(wedges[2, ] / wedges[1, ])
    return(data.frame(V = wedges[1, ], T = wedges[2, ]))
}

# Opsah'l clustering coefficient
opsahl.transitivity <- function(
    bigraph, node.type = 0, type = 'global',
    vids = which(V(bigraph)$type == node.type)
    ) {
    twomode.transitivity(
        bigraph = bigraph, node.type = node.type, type = type,
        wedges.fn = opsahl.wedges, vids = vids)
}

# Inclusive clustering coefficient
incl.transitivity <- function(
    bigraph, node.type = 0, type = 'global',
    vids = which(V(bigraph)$type == node.type)
    ) {
    twomode.transitivity(
        bigraph = bigraph, node.type = node.type, type = type,
        wedges.fn = incl.wedges, vids = vids)
}

# Exclusive clustering coefficient
excl.transitivity <- function(
    bigraph, node.type = 0, type = 'global',
    vids = which(V(bigraph)$type == node.type)
    ) {
    twomode.transitivity(
        bigraph = bigraph, node.type = node.type, type = type,
        wedges.fn = excl.wedges, vids = vids)
}

# Wedge-dependent local clustering: distributions or functions thereof
wedge.transitivity <- function(
    bigraph, node.type = 0, FUN = identity, wedges.fn = opsahl.wedges, ...
    ) {
    wedges <- twomode.transitivity(
        bigraph = bigraph, node.type = node.type, type = 'wedges',
        wedges.fn = wedges.fn)
    return(sapply(1:max(wedges[1, ]), function(k) {
        wh.k <- which(wedges[1, ] == k)
        unname(FUN(wedges[2, wh.k] / wedges[1, wh.k]))
    }))
}
