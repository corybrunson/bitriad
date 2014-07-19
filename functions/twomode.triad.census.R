# FUNCTION: Triad census for undirected networks (only 4 isomorphism classes)
simple.triad.census <- function(graph, rcnames = FALSE) {
    tc <- triad.census(as.directed(graph))
    stopifnot(sum(tc) == choose(vcount(graph), 3))
    if(is.nan(tc[1])) tc[1] <- choose(vcount(graph), 3) - sum(tc, na.rm = TRUE)
    stc <- tc[c(1, 3, 11, 16)]
    if(rcnames) names(stc) <- 0:3
    return(stc)
}

# FUNCTION: Two-mode dyad census (equivalently, distribution of edge weights
# in the one-mode projection when edges are weighted by shared event count)
twomode.dyad.census <- function(bigraph, type = 0) {
    graph <- bipartite.projection(bigraph, multiplicity = TRUE)[[1 + type]]
    disconnected <- choose(vcount(graph), 2) - ecount(graph)
    return(c('0' = if(disconnected == 0) NULL else disconnected,
             table(E(graph)$weight)))
}

# FUNCTION: Produce a one-mode projection onto nodes of the given type
# so that the names of the projection nodes are the indices of their
# counterparts in the original bigraph
onemode.projection <- function(bigraph, type = 0, name = 'name') {
    if(name == 'id') V(bigraph)$name <- V(bigraph)
    return(bipartite.projection(bigraph, multiplicity = TRUE)[[1 + type]])
}

# FUNCTION: Tally disconnected triples with a single edge of weight x
one.tied.triads <- function(graph) {
    # Create a data frame of weights and number of nonadjacent nodes
    counts <- data.frame(
        x = E(graph)$weight,
        n = vcount(graph) - sapply(1:ecount(graph), function(i) {
            length(unique(unlist(neighborhood(graph, 1, get.edge(graph, i)))))
        })
    )
    # Return the aggregated data frame
    return(aggregate(n ~ x, data = counts, FUN = sum))
}

# FUNCTION: Count the secondary nodes shared by the given primary nodes
share.weight <- function(bigraph, vids, name = 'name') {
    if(name == 'id') vids <- as.numeric(vids)
    length(Reduce(intersect, neighborhood(bigraph, 1, as.numeric(vids))))
}

# FUNCTION: Return the weight of the edge between two nodes, or else zero
edge.weight <- function(graph, vp) {
    id <- get.edge.ids(graph, vp)
    if(id == 0) 0 else E(graph)$weight[id]
}

# FUNCTION: Count open and closed wedges, subtracting triad weight if nonzero
connected.triples <- function(
    bigraph, type = 0,
    # Construct the one-mode projection if it's not already prepared
    graph = onemode.projection(bigraph, type = type, name = 'id')
) {
    trips <- do.call(rbind, lapply(1:vcount(graph), function(i) {
        nbhd <- neighborhood(graph, 1, i)[[1]]
        # Skip nodes with not enough neighbors
        if(length(nbhd) < 2) return(NULL)
        # horizontal array of pairs of neighbors of i
        v <- combn(setdiff(nbhd, i), 2)
        # vector of triad weights
        w <- sapply(1:dim(v)[2], function(j) {
            share.weight(bigraph, V(graph)$name[c(i, v[, j])])
        })
        # horizontal array of sorted triples of edge weights
        ew <- sapply(1:dim(v)[2], function(j) {
            sort(c(edge.weight(graph, c(i, v[1, j])),
                   edge.weight(graph, c(i, v[2, j])),
                   edge.weight(graph, c(v[1, j], v[2, j]))),
                 decreasing = TRUE)
        })
        # vertical array of pair and triad weights
        return(data.frame(x = ew[1, ] - w, y = ew[2, ] - w,
                          z = ew[3, ] - w, w = w))
    }))
    return(aggregate(n ~ x * y * z * w, FUN = sum,
                     data = cbind(trips,
                                  n = 1 - (trips$z + trips$w > 0) * 2 / 3)))
}

# FUNCTION: Triad census for two-mode networks
# (Iterates over nodes)
twomode.triad.census1 <- function(bigraph, type = 0, rcnames = FALSE,
                                  verbose = FALSE) {
    # Drop trivial cases
    if(vcount(bigraph) == 0) return(matrix(0, nr = 0, nc = 0))
    # Create one-mode projection
    graph <- onemode.projection(bigraph, type = type, name = 'id')
    
    # Find maximum values of x and of w
    max.x <- max(E(graph)$weight)
    # Initialize matrix (overestimating the number of columns)
    C <- as.data.frame(matrix(0, nr = choose(max.x + 3, 3), nc = max.x + 1))
    
    # Tally one-tied triads
    ot <- one.tied.triads(graph)
    # Insert the totals at the proper entries of C
    # (No repeats, so no information loss)
    C[sapply(ot$x, function(x) partition.position(c(x, 0, 0))) + 1, 1] <- ot$n
    if(verbose) print('One-tied triads tallied')
    
    # Tally connected triples (be sure to specify consistent type)
    ct <- connected.triples(bigraph, type = type, graph = graph)
    # Trim any unnecessary columns
    max.w <- max(ct$w)
    C <- C[, 1:(max.w + 1)]
    # For each value of w:
    for(w in 0:max.w) {
        if(verbose) print(paste('Tallying weight-', w, ' connected triples',
                                sep = ''))
        # Which rows have weight w?
        rs <- which(ct$w == w)
        # Insert the totals at the proper rows in column w + 1 of C
        # (No repeats, so no information loss)
        C[sapply(rs, function(i) {
            partition.position(as.numeric(ct[i, 1:3])) + 1
        }), w + 1] <- ct$n[rs]
    }
    if(verbose) print('Connected triples tallied')
    
    # The remaining triads share no secondary nodes; count them as empty
    # (No triads should have yet been counted as empty)
    C[1, 1] <- choose(vcount(graph), 3) - sum(C)
    # Clear names
    colnames(C) <- NULL
    if(rcnames) {
        colnames(C) <- 0:(ncol(C) - 1)
        rownames(C) <- sapply(0:(nrow(C) - 1), function(i) paste(
            '(', paste(position.partition(i, k = 3), collapse = ','),
            ')', sep = ''))
    }
    return(as.matrix(C))
}

# FUNCTION: Tally triples with exactly two edges among them
two.tied.triads <- function(graph) {
    # List of open wedges (shortest paths of length 2) up to reversal
    p2 <- do.call(cbind, lapply(V(graph)[1:(vcount(graph) - 1)], function(v) {
        d2 <- as.numeric(V(graph)[
            which(shortest.paths(graph, v, (v + 1):vcount(graph),
                                 weights = NA) == 2) + v
            ])
        gasp <- get.all.shortest.paths(graph, v, d2, weights = NA)[[1]]
        do.call(cbind, gasp[sapply(gasp, length) == 3])
    }))
    # Horizontal array of sorted edge weight pairs
    if(is.null(p2)) return(NULL) else  wedges <- sapply(
        1:dim(p2)[2],
        function(j) sort(c(edge.weight(graph, c(p2[1, j], p2[2, j])),
                           edge.weight(graph, c(p2[2, j], p2[3, j]))),
                         decreasing = TRUE))
    # Make wedges into a data frame
    wedges <- data.frame(x = wedges[1, ], y = wedges[2, ], n = 1)
    # Return the aggregated data frame
    return(aggregate(n ~ x * y, FUN = sum,
                     data = cbind(wedges, n = rep(1, n = dim(wedges)[1]))))
}

# FUNCTION: Tally triangles, subtracting triad weight if nonzero
three.tied.triads <- function(
    bigraph, type = 0,
    # Construct the one-mode projection if it's not already prepared
    graph = onemode.projection(bigraph, type = type, name = 'id')
) {
    # Triangles are 3-cliques in the one-mode projection
    t <- do.call(cbind, cliques(graph, min = 3, max = 3))
    # If there are no triangles then return an empty list
    if(is.null(t)) return(NULL)
    # Vector of triad weights
    w <- sapply(1:dim(t)[2], function(j) {
        share.weight(bigraph, V(graph)$name[c(t[1, j], t[2, j], t[3, j])])
    })
    # Horizontal array of sorted triples of edge weights
    ew <- sapply(1:dim(t)[2], function(j) {
        sort(c(edge.weight(graph, c(t[1, j], t[2, j])),
               edge.weight(graph, c(t[2, j], t[3, j])),
               edge.weight(graph, c(t[1, j], t[3, j]))),
             decreasing = TRUE)
    })
    tris <- data.frame(x = ew[1, ] - w, y = ew[2, ] - w,
                       z = ew[3, ] - w, w = w)
    return(aggregate(n ~ x * y * z * w, data = cbind(tris, n = 1), FUN = sum))
}

# FUNCTION: Triad census for two-mode networks
# (Iterates over paths of length 2)
twomode.triad.census2 <- function(bigraph, type = 0, rcnames = FALSE,
                                  verbose = FALSE) {
    # Drop trivial cases
    if(vcount(bigraph) == 0) return(matrix(0, nr = 0, nc = 0))
    # Create one-mode projection
    graph <- onemode.projection(bigraph, type = type, name = 'id')
    # Trivial case
    if(ecount(graph) == 0) return(matrix(choose(vcount(graph), 3),
                                         nr = 1, nc = 1))
    
    # Find maximum values of x and of w
    max.x <- max(E(graph)$weight)
    # Initialize matrix (overestimating the number of columns)
    C <- as.data.frame(matrix(0, nr = choose(max.x + 3, 3), nc = max.x + 1))
    
    # Tally one-tied triads
    ot <- one.tied.triads(graph)
    # Insert the totals at the proper entries of C
    # (Aggregated, so no repeats, so no information loss)
    if(length(ot) > 0) C[sapply(ot$x, function(x) {
        partition.position(c(x, 0, 0))
    }) + 1, 1] <- ot$n
    if(verbose) print('One-tied triads tallied')
    
    # Tally two-tied triads
    tt <- two.tied.triads(graph)
    # Insert the totals at the proper entries of C
    # (Aggregated, so no repeats, so no information loss)
    if(!is.null(tt)) C[sapply(1:dim(tt)[1], function(i) {
        partition.position(c(tt[i, 1], tt[i, 2], 0))
    }) + 1, 1] <- tt$n
    if(verbose) print('Two-tied triads tallied')
    
    # Tally triangles
    tht <- three.tied.triads(bigraph, type = type, graph = graph)
    # If there are any...
    if(!is.null(tht)) {
        # Trim any unnecessary columns
        max.w <- max(tht$w)
        C <- C[, 1:(max.w + 1)]
        # For each value of w:
        for(w in 0:max.w) {
            if(verbose) print(paste('Tallying weight-', w,
                                    ' three-tied triads', sep = ''))
            # Which rows have weight w?
            rs <- which(tht$w == w)
            # Insert the totals at the proper rows in column w + 1 of C
            # (No repeats, so no information loss)
            if(length(rs) > 0) C[sapply(rs, function(i) {
                partition.position(as.numeric(tht[i, 1:3])) + 1
            }), w + 1] <- tht$n[rs]
        }
    }
    if(verbose) print('Three-tied triads tallied')
    
    # The remaining triads share no secondary nodes; count them as empty
    # (No triads should have yet been counted as empty)
    C[1, 1] <- choose(vcount(graph), 3) - sum(C)
    # Reality check: The total triad tally should equal |V(graph)|-choose-3
    stopifnot(sum(C) == choose(vcount(graph), 3))
    # Clear names
    colnames(C) <- NULL
    if(rcnames) {
        colnames(C) <- 0:(ncol(C) - 1)
        rownames(C) <- sapply(0:(nrow(C) - 1), function(i) paste(
            '(', paste(position.partition(i, k = 3), collapse = ','),
            ')', sep = ''))
    }
    return(as.matrix(C))
}

# Trials on small networks indicate that version 2 is substantially faster;
# version 1 is retained as a check
twomode.triad.census <- twomode.triad.census2

# Derive clustering coefficients from two-mode triad census
tmtc2cc <- function(tc, S.fn, F.fn, num.denom = FALSE) {
    if(dim(tc)[1] * dim(tc)[2] == 0) return(NA)
    S.c <- sum(sapply(1:dim(tc)[2], function(j) sapply(
        1:dim(tc)[1], function(i) {
            if(tc[i, j] == 0) 0 else
                S.fn(position.partition(3, i - 1), j - 1) * tc[i, j]})))
    F.c <- sum(sapply(1:dim(tc)[2], function(j) sapply(
        1:dim(tc)[1], function(i) {
            if(tc[i, j] == 0) 0 else
                F.fn(position.partition(3, i - 1), j - 1) * tc[i, j]})))
    return(if(num.denom) c(S.c, S.c + F.c) else S.c / (S.c + F.c))}

# Classical clustering coefficient on the one-mode projection
# tc must be a matrix with rows indexed as partition.position
tmtc2C <- function(tc, num.denom = FALSE, by.tri = FALSE) tmtc2cc(
    tc,
    function(L, w) ifelse(by.tri, 1, 3) * ((L[3] > 0) | (w > 0)),
    function(L, w) ((L[2] > 0) & (L[3] == 0) & (w == 0)),
    num.denom = num.denom)

# Global Opsahl clustering coefficient
# (agrees with bipartite.transitivity)
# tc must be a matrix with rows indexed as partition.position
tmtc2CO <- function(tc, num.denom = FALSE, by.tri = FALSE) tmtc2cc(
    tc,
    function(L, w) ifelse(
        by.tri,
        L[1] * L[2] * L[3] +
            (L[1] * L[2] + L[1] * L[3] + L[2] * L[3]) * w +
            sum(L) * w * (w - 1) +
            w * (w - 1) * (w - 2),
        L[1] * L[2] * (L[3] + w > 0) + L[1] * L[3] + L[2] * L[3] +
            L[1] * w * (L[2] > 0 | w > 1) + L[1] * w * (L[3] > 0 | w > 1) +
            L[2] * w + L[2] * w * (L[3] > 0 | w > 1) +
            2 * L[3] * w +
            2 * choose(w, 2) * max(3 * (w > 2), length(which(L > 0)))),
    function(L, w) L[1] * L[2] * (L[3] + w == 0) +
        L[1] * (L[2] == 0 & w == 1) + L[1] * (L[3] == 0 & w == 1) +
        L[2] * (L[3] == 0 & w == 1) +
        2 * choose(w, 2) * min(3 * (w == 2), length(which(L == 0))),
    num.denom = num.denom)

# Global inclusive clustering coefficient
# (existence of not necessarily induced 4-paths and 6-cycles)
# tc must be a matrix with rows indexed as partition.position
tmtc2Cin <- function(tc, num.denom = FALSE, by.tri = FALSE) tmtc2cc(
    tc,
    function(L, w) ifelse(by.tri, 1, 3) * (length(which(L > 0)) + w > 2),
    function(L, w) (L[2] > 0 & L[3] == 0 & w == 0) +
        2 * (L[1] > 0 & L[2] == 0 & w == 1) +
        3 * (L[1] == 0 & w == 2),
    num.denom = num.denom)

# Global exclusive clustering coefficient
# (existence of induced 4-paths and 6-cycles)
# (agrees with exclusive.transitivity)
# tc must be a matrix with rows indexed as partition.position
tmtc2Cex <- function(tc, num.denom = FALSE, by.tri = FALSE) tmtc2cc(
    tc,
    function(L, w) ifelse(by.tri, 1, 3) * (L[3] > 0),
    function(L, w) ((L[2] > 0) & (L[3] == 0)), num.denom)

# Pairwise weight–resolved exclusive clustering
# tc must be a matrix with rows indexed as partition.position
tmtc2Cexij <- function(tc, i, j, num.denom = FALSE, by.tri = FALSE) tmtc2cc(
    tc,
    function(L, w) ifelse(by.tri, 1, 3) * ((L[2] >= i) & (L[3] >= j)),
    function(L, w) ((L[2] >= i) & (L[3] < j)), num.denom)

# Triad weight–resolved exclusive clustering
# tc must be a matrix with rows indexed as partition.position
tmtc2Cexw <- function(tc, ww, num.denom = FALSE, by.tri = FALSE) tmtc2cc(
    tc,
    function(L, w) ifelse(by.tri, 1, 3) * ((L[3] > 0) & (w == ww)),
    function(L, w) ((L[2] > 0) & (L[3] == 0) & (w == ww)), num.denom)

# Make a list of matrices all the same dimensions by appending zeroes
sync.mat <- function(lst) {
    sync.dim <- apply(sapply(lst, dim), 1, max)
    return(lapply(lst, function(mat) {
        cbind(rbind(mat,
                    matrix(0, nr = sync.dim[1] - dim(mat)[1],
                           nc = dim(mat)[2])),
              matrix(0, nr = sync.dim[1], nc = sync.dim[2] - dim(mat)[2]))
    }))
}

# Take the quotient of the two-mode triad census by structural equivalence
# (4, 2) matrix with rows labeled by (0, 1) lambas and columns by (0, 1) ws
tmtc2setc <- function(tmtc) {
    # Trivial cases
    if(sum(tmtc) == 0) return(matrix(0, nr = 4, nc = 2))
    if(all(dim(tmtc) == 1)) return(matrix(c(tmtc[1, 1], rep(0, 7)),
                                          nr = 4, nc = 2))
    # No. edges (1, 2, or 3) induced by each nonempty lambda with w = 0
    pw.counts <- sapply(1:(dim(tmtc)[1] - 1), function(i) {
        length(which(position.partition(i, k = 3) > 0))
    })
    # Which rows connect 1, 2, and 3 pairs
    wh <- lapply(1:3, function(i) which(pw.counts == i) + 1)
    return(matrix(c(
        # Empty triads
        tmtc[1, 1],
        # No triad event; 1, 2, and 3 pairs connected
        sum(tmtc[wh[[1]], 1]),
        sum(tmtc[wh[[2]], 1]),
        sum(tmtc[wh[[3]], 1]),
        # Committees with no pairwise events
        sum(tmtc[1, 2:dim(tmtc)[2]]),
        # Triad event; 1, 2, and 3 pairs connected
        sum(tmtc[wh[[1]], 2:dim(tmtc)[2]]),
        sum(tmtc[wh[[2]], 2:dim(tmtc)[2]]),
        sum(tmtc[wh[[3]], 2:dim(tmtc)[2]])
    ), nr = 4, nc = 2))
}

twomode.structural.triad.census <- function(bigraph, type = 0, rcnames = FALSE,
                                            verbose = FALSE) {
    tmtc2setc(twomode.triad.census(bigraph, type, rcnames, verbose))
}

# Recover the simple triad census from the two-mode triad census
tmtc2stc <- function(tmtc) {
    # Trivial cases
    if(sum(tmtc) == 0) return(rep(0, 4))
    if(all(dim(tmtc) == 1)) return(c(tmtc[1, 1], 0, 0, 0))
    # Number of edges (1, 2, or 3) induced by each lambda other than c(0, 0, 0)
    pw.counts <- sapply(1:(dim(tmtc)[1] - 1), function(i) {
        length(which(position.partition(i, k = 3) > 0))
    })
    return(c(
        # Empty triads all have lambda = c(0, 0, 0), w = 0
        tmtc[1, 1],
        # Dyads
        sum(tmtc[which(pw.counts == 1) + 1, 1]),
        # Intransitive wedges
        sum(tmtc[which(pw.counts == 2) + 1, 1]),
        # Triangles, including all columns w > 0
        sum(tmtc[which(pw.counts == 3) + 1, 1]) +
            ifelse(dim(tmtc)[2] == 1, 0, sum(tmtc[, 2:dim(tmtc)[2]]))))
}

# Subgraph at a given set of actors and all events attended by at least two
schedule <- function(bigraph, v) {
    stopifnot(all(V(bigraph)$type[v] == 0))
    events <- unlist(neighborhood(bigraph, 1, v))
    tab <- table(events)
    coattended <- as.numeric(names(tab)[tab > 1])
    return(induced.subgraph(bigraph, c(v, coattended)))
}

# Class of a triad
triad.class <- function(bigraph, v) {
    stopifnot(all(V(bigraph)$type[v] == 0) & length(v) == 3)
    pairs <- neighborhood(bigraph, 1, v)
    w <- length(which(table(unlist(pairs)) > 2))
    lambda <- sort(sapply(pairs, length) - 1 - w, decreasing = TRUE)
    return(list(lambda = lambda, w = w))
}
