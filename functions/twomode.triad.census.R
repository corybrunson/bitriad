# FUNCTION: Two-mode dyad census (equivalently, distribution of edge weights
# in the one-mode projection when edges are weighted by shared event count)
twomode.dyad.census <- function(bigraph, type = 1) {
    graph <- bipartite.projection(bigraph, multiplicity = TRUE)[[1 + type]]
    disconnected <- choose(vcount(graph), 2) - ecount(graph)
    return(c('0' = if(disconnected == 0) NULL else disconnected,
             table(E(graph)$weight)))
}

# FUNCTION: Produce a one-mode projection onto nodes of the given type
# so that the names of the projection nodes are the indices of their
# counterparts in the original bigraph
onemode.projection <- function(bigraph, type = 1, name = 'name') {
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
    bigraph, type = 1,
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

# FUNCTION: Maximum triad weight
max.triad.weight <- function(bigraph, type = 1) {
    b = bigraph
    v = vcount(b) + 1
    n = 0
    # Iteratively delete primary nodes of degree < n and
    # secondary nodes of degree < 3 until none remain
    while(vcount(b) < v) {
        v <- vcount(b)
        n <- n + 1
        w = vcount(b) + 1
        while(vcount(b) < w) {
            w <- vcount(b)
            b <- delete.vertices(b,
                                 which((degree(b) < n) & (V(b)$type == type)))
            b <- delete.vertices(b,
                                 which((degree(b) < 3) & (V(b)$type != type)))
        }
    }
    # The max is one less than what it took to lose everything
    return(n)
}

# FUNCTION: Triad census for two-mode networks
# (Uses connected.triples; iterates over nodes)
twomode.triad.census1 <- function(bigraph, type = 1, rowcolnames = FALSE) {
    # Drop trivial cases
    if(vcount(bigraph) == 0) return(matrix(0, nr = 0, nc = 0))
    # Create one-mode projection
    graph <- onemode.projection(bigraph, type = type, name = 'id')
    
    # Find maximum values of x and of w
    max.x <- max(E(graph)$weight)
    max.w <- max.triad.weight(bigraph)
    # Initialize matrix
    C <- as.data.frame(matrix(0, nr = choose(max.x + 3, 3), nc = max.w + 1))
    
    # Tally one-tied triads
    ot <- one.tied.triads(graph)
    # Insert the totals at the proper entries of C
    # (No repeats, so no information loss)
    C[sapply(ot$x, function(x) partition.position(c(x, 0, 0))) + 1, 1] <- ot$n
    
    # Tally connected triples (be sure to specify consistent type)
    ct <- connected.triples(bigraph, type = type, graph = graph)
    for(w in 0:max.w) {
        # Which rows have weight w?
        rs <- which(ct$w == w)
        # Insert the totals at the proper rows in column w + 1 of C
        # (No repeats, so no information loss)
        C[sapply(rs, function(i) {
            partition.position(as.numeric(ct[i, 1:3])) + 1
        }), w + 1] <- ct$n[rs]
    }
    
    # The remaining triads share no secondary nodes; count them as empty
    # (No triads should have yet been counted as empty)
    C[1, 1] <- choose(vcount(graph), 3) - sum(C)
    # Clear names
    colnames(C) <- NULL
    if(rowcolnames) {
        colnames(C) <- 0:(ncol(C) - 1)
        rownames(C) <- sapply(0:(nrow(C) - 1), function(i) paste(
            '(', paste(position.partition(i, k = 3), collapse = ','),
            ')', sep = ''))
    }
    return(as.matrix(C))
}

# FUNCTION: Tally triples with exactly two edges among them
two.tied.triads <- function(graph) {
    
}

# FUNCTION: Tally triangles, subtracting triad weight if nonzero
three.tied.triads <- function(
    bigraph, type = 1,
    # Construct the one-mode projection if it's not already prepared
    graph = onemode.projection(bigraph, type = type, name = 'id')
) {
    
}

# FUNCTION: Triad census for two-mode networks
# (Mimics original; iterates over paths of length 2)
twomode.triad.census2 <- function(bigraph, type = 1, rowcolnames = FALSE) {
    # Drop trivial cases
    if(vcount(bigraph) == 0) return(matrix(0, nr = 0, nc = 0))
    # Create one-mode projection
    graph <- onemode.projection(bigraph, type = type, name = 'id')
    
    # Find maximum values of x and of w
    max.x <- max(E(graph)$weight)
    max.w <- max.triad.weight(bigraph)
    # Initialize matrix
    C <- as.data.frame(matrix(0, nr = choose(max.x + 3, 3), nc = max.w + 1))
    
    # Tally one-tied triads
    ot <- one.tied.triads(graph)
    # Insert the totals at the proper entries of C
    # (No repeats, so no information loss)
    C[sapply(ot$x, function(x) partition.position(c(x, 0, 0))) + 1, 1] <- ot$n
    
    # Tally two-tied triads
    
    # The remaining triads share no secondary nodes; count them as empty
    # (No triads should have yet been counted as empty)
    C[1, 1] <- choose(vcount(graph), 3) - sum(C)
    # Clear names
    colnames(C) <- NULL
    if(rowcolnames) {
        colnames(C) <- 0:(ncol(C) - 1)
        rownames(C) <- sapply(0:(nrow(C) - 1), function(i) paste(
            '(', paste(position.partition(i, k = 3), collapse = ','),
            ')', sep = ''))
    }
    return(as.matrix(C))
}

# Pick the better implementation (2 once it's completed)
twomode.triad.census <- twomode.triad.census1
