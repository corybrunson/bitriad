# igraph library
library(igraph)


# Indexing scheme

# k-combination of [0 : (n - 1)] corresponding to a given k-part partition
partition.combination <- function(lambda) lambda + (length(lambda) - 1):0

# k-part partition corresponding to a given k-combination of [0 : (n - 1)]
combination.partition <- function(vec) vec - (length(vec) - 1):0

# Rev-lex position of a given k-combination of [0 : (n - 1)], k = length(vec)
combination.position <- function(vec) {
  stopifnot(!is.unsorted(rev(vec), strictly = TRUE))
  sum(choose(rev(vec), 1:length(vec)))
}

# k-combination at a given rev-lex position
position.combination <- function(k, i) {
  vec <- c()
  N <- i
  for(j in k:1) {
    c <- j - 1
    while(choose(c + 1, j) <= N) c <- c + 1
    vec <- c(vec, c)
    N <- N - choose(c, j)
  }
  return(vec)
}

# Rev-lex position of a given k-part partition
partition.position <- function(lambda) {
  combination.position(partition.combination(lambda))
}

# k-part partition at a given rev-lex position
position.partition <- function(k, i) {
  combination.partition(position.combination(k, i))
}


# Miscellaneous graph-theoretic

# bipartite cliques (of at least m papers and n authors) in clumps
bipartite.cliques <- function(bigraph, m, n) {
  stopifnot(is.bipartite(bigraph))
  vc <- vcount(bigraph)
  bg <- delete.vertices(bigraph, v = c(
    which(degree(bigraph) < m & !V(bigraph)$type),
    which(degree(bigraph) < n & V(bigraph)$type)
  ))
  while(vcount(bg) < vc) {
    vc <- vcount(bg)
    bg <- delete.vertices(bg, v = c(
      which(degree(bg) < m & !V(bg)$type),
      which(degree(bg) < n & V(bg)$type)
    ))
  }
  cl <- clusters(bg)
  lapply(1:cl$no,
         function(i) induced.subgraph(bg, v = which(cl$membership == i)))
}


# Candidate clustering coefficients

# approximate clustering corrected for assortativity (Soffer–Vasquez 2005)
assortative.transitivity <- function(graph, vids = V(graph), type = 'global') {
  k <- degree(graph)
  t <- transitivity(graph, vids = vids, type = 'local') * choose(k[vids], 2)
  Omega <- sapply(neighborhood(graph, 1, nodes = vids), function(vec) {
    k0 <- length(vec) - 1
    if(k0 < 1) NaN else
    floor(sum(sapply(vec[-1], function(v) min(k0, k[v])) - 1) / 2)
  })
  if(type == 'local') t / Omega else
  if(type == 'global') sum(t, na.rm = TRUE) / sum(Omega, na.rm = TRUE) else
  return(list(stratified = data.frame(Omega = Omega, t = t),
              global = sum(t, na.rm = TRUE) / sum(Omega, na.rm = TRUE)))
}

# bipartite triadic closure (Opsahl 2012)
# Proportion of L_4s that lie in C_6s; differs from mine in that
# (a) multiple L_4s through the same three primary vertices are counted and
# (b) chords (edges between nonconsecutive vertices in L_4) are permitted.
bipartite.transitivity <- function(
  bigraph, type = 'global', vids = which(V(bigraph)$type == 1),
  status = FALSE
) {
  # Check that nodes are of common type (requires attribute 'type')
  stopifnot(all(V(bigraph)$type[vids] == 1) |
            !any(V(bigraph)$type[vids] == 1))
  # If global, need to look at all vertices
  vs <- if(type != 'local') which(V(bigraph)$type == 1) else vids
  if(status) {
    pb <- txtProgressBar(min = 0, max = length(vs), style = 3)
    step <- 0}
  # Array of L_4s
  L4 <- matrix(unlist(lapply(vs, function(v) {
    # Identify neighbors of v
    n1 <- setdiff(neighborhood(bigraph, order = 1, nodes = v)[[1]], v)
    if(length(n1) < 2) return(matrix(rep(NA, 6), nr = 6)[, c()])
    # Identify the neighborhoods of vertices in n1
    n1s <- lapply(n1, function(w) setdiff(
             neighborhood(bigraph, order = 1, nodes = w)[[1]], c(v, w)))
    # Expand each L_2 centered at v to all its L_4s,
    # with v between the endpoints and an indicator if the L_4 is closed
    n1p <- combn(1:length(n1), 2)
    # For each pair of neighbors of v (counted once)
    lap <- lapply(1:dim(n1p)[2], function(j) {
      # If the first node w has a nonzero neighborhood besides v
      if(length(n1s[[n1p[1, j]]]) == 0)
      matrix(rep(NA, 6), nr = 6)[, c()] else
      # For each node y in the neighborhood of w without v
      lapply(n1s[[n1p[1, j]]], function(y) {
        # If the second node x (of the pair with w of neighbors of v)
        # has a nonzero neighborhood besides v and y
        if(length(setdiff(n1s[[n1p[2, j]]], y)) == 0)
        matrix(rep(NA, 6), nr = 6)[, c()] else
        # Include each 4-path (y, w, v, x, z)
        # where z is a neighbor of x besides v and y
        rbind(y, n1[n1p[1, j]], v, n1[n1p[2, j]],
              setdiff(n1s[[n1p[2, j]]], y),
          # And an indicator of whether the 4-path is closed
          sapply(setdiff(n1s[[n1p[2, j]]], y), function(z) length(
            setdiff(intersect(neighborhood(bigraph, 1, nodes = y)[[1]],
                              neighborhood(bigraph, 1, nodes = z)[[1]]),
                    c(n1[n1p[1, j]], n1[n1p[2, j]]))) > 0))
      })
    })
    if(status) {
      step <- step + 1
      setTxtProgressBar(pb, step)}
    return(lap)
  })), nr = 6)
  if(status) close(pb)
  if(type != 'local') bC <- length(which(as.logical(L4[6, ]))) / dim(L4)[2]
  if(type != 'global') {
    l4 <- hist(L4[3, ], breaks = 0:vcount(bigraph) + .5)$counts
    c6 <- hist(L4[3, L4[6, ] == 1], breaks = 0:vcount(bigraph) + .5)$counts
    bc <- c6[vids] / l4[vids]
  }
  if(type == 'global') return(bC) else
  if(type == 'local') return(c6[vids] / l4[vids]) else
  return(list(stratified = data.frame(l4 = l4[vids], c6 = c6[vids]),
              global = bC))
}

# Hall's criterion for the existence of a system of distinct representatives
sdr.criterion <- function(lst) all(sapply(0:(2 ^ length(lst) - 1), function(i) {
  w <- which(intToBits(i) == 1)
  length(unique(unlist(lst[w]))) >= length(w)
}))

# Inclusive clustering (Brunson)
# (first candidate clustering coefficient)
# (existence of not necessarily induced 4-paths and 6-cycles)
# (does not satisfy requirements laid out in abstract)
inclusive.transitivity <- function(
  bigraph, type = 'global', vids = V(bigraph)[V(bigraph)$type == 1]
) {
  # Check that nodes are of common type (requires attribute 'type')
  stopifnot(all(V(bigraph)$type[vids] == 1) |
            !any(V(bigraph)$type[vids] == 1))
  # If global, need to look at all vertices
  vs <- if(type != 'local') V(bigraph)[V(bigraph)$type == 1] else vids
  # Array of vees with distinct collaborations
  # and an indicator of triadic collaborations below
  L4 <- matrix(unlist(lapply(vs, function(v) {
    # Identify nodes of separation (exactly) 1 and 2 from v
    n1 <- setdiff(neighborhood(bigraph, order = 1, nodes = v)[[1]], v)
    n2 <- setdiff(neighborhood(bigraph, order = 2, nodes = v)[[1]], c(n1, v))
    if(length(n2) < 2) return(matrix(NA, nr = 4, nc = 0))
    # Identify pairs of nodes in n2
    p <- combn(n2, 2)
    # Identify which of these pairs form L_4s and, of these, which form C_6s,
    # stored as a (2,#pairs) T/F matrix
    lc <- sapply(1:dim(p)[2], function(j) {
      n1s <- neighborhood(bigraph, order = 1, nodes = p[1:2, j])
      m <- intersect(n1s[[1]], n1s[[2]])
      # If only one affiliation links either of them to v then no L_4 or C_6
      if(length(intersect(n1, unique(unlist(n1s)))) == 1) c(F, F) else
      # Otherwise L_4 and, provided Hall criterion is met, C_6
        c(T, sdr.criterion(list(intersect(n1, n1s[[1]]),
                                intersect(n1, n1s[[2]]),
                                m)))
    })
    # Restrict p to only those pairs that form L_4s with v at the center
    wh <- which(lc[1, ])
    # Return the vector of one end, v, other end, indicator of C_6
    if(length(wh) == 0) matrix(NA, nr = 4, nc = 0) else
      rbind(p[1, wh], v, p[2, wh], lc[2, wh])
  })), nr = 4)
  colnames(L4) <- NULL
  if(type != 'local') nC <- length(which(as.logical(L4[4, ]))) / dim(L4)[2]
  if(type != 'global') {
    # Histograms of L_4s and C_6s centered at each vertex,
    # tallied only for vertices of given type though hist ranges over all
    l4 <- hist(L4[2, ], breaks = 0:vcount(bigraph) + .5)$counts
    c6 <- hist(L4[2, L4[4, ] == 1], breaks = 0:vcount(bigraph) + .5)$counts
  }
  if(type == 'global') return(nC) else
  if(type == 'local') return(c6[vids] / l4[vids]) else
  return(list(stratified = data.frame(l4 = l4[vids], c6 = c6[vids]),
              global = nC))
}

# Exclusive clustering (Brunson)
exclusive.transitivity <- function(
  bigraph, type = 'global', vids = V(bigraph)[V(bigraph)$type == 1]
) {
  # Check that nodes are of common type (requires attribute 'type')
  stopifnot(all(V(bigraph)$type[vids] == 1) | !any(V(bigraph)$type[vids] == 1))
  # If global, need to look at all vertices
  vs <- if(type != 'local') V(bigraph)[V(bigraph)$type == 1] else vids
  # Array of vees with pairwise exclusive collaborations
  # and an indicator of triadic exclusive collaborations below
  L4 <- matrix(unlist(lapply(vs, function(v) {
    # Identify nodes of separation (exactly) 1 and 2 from v
    n1 <- setdiff(neighborhood(bigraph, order = 1, nodes = v)[[1]], v)
    n2 <- setdiff(neighborhood(bigraph, order = 2, nodes = v)[[1]], c(n1))
    if(length(n2) < 2) return(matrix(rep(NA, 4), nr = 4)[, c()])
    # Identify pairs of nodes in n2 that share no immediate neighbors,
    # regardless of how many neighbors they share with v
    n1s <- lapply(n2, function(w) setdiff(
             neighborhood(bigraph, order = 1, nodes = w)[[1]], w))
    p <- combn(1:length(n2), 2)
    p <- as.matrix(p[, which(sapply(1:dim(p)[2], function(j) {
      (length(setdiff(intersect(n1s[[p[1, j]]], n1), n1s[[p[2, j]]])) > 0) &
      (length(setdiff(intersect(n1s[[p[2, j]]], n1), n1s[[p[1, j]]])) > 0)
    }))], nr = 2)
    if(length(p) == 0) return(matrix(rep(NA, 4), nr = 4)[, c()])
    # Identify which of these pairs share a neighbor not shared with v
    cl <- sapply(1:dim(p)[2], function(j) {
      length(setdiff(intersect(n1s[[p[1, j]]], n1s[[p[2, j]]]), n1)) > 0
    })
    # Substitute the bigraph vertex ids for the enumeration scheme in p
    p <- matrix(n2[p], nr = 2)
    # Return the vector of one end, v, other end, indicator of C_6
    rbind(p[1, ], v, p[2, ], cl)
  })), nr = 4)
  colnames(L4) <- NULL
  if(type != 'local') xC <- length(which(as.logical(L4[4, ]))) / dim(L4)[2]
  if(type != 'global') {
    # Histograms of L_4s and C_6s centered at each vertex,
    # tallied only for vertices of given type though histogram ranges over all
    l4 <- hist(L4[2, ], breaks = 0:vcount(bigraph) + .5)$counts
    c6 <- hist(L4[2, L4[4, ] == 1], breaks = 0:vcount(bigraph) + .5)$counts
  }
  if(type == 'global') return(xC) else
  if(type == 'local') return(c6[vids] / l4[vids]) else
  return(list(stratified = data.frame(l4 = l4[vids], c6 = c6[vids]),
              global = xC))
}


# Triad census functions

simple.triad.census <- function(graph) {
  tc <- triad.census(as.directed(graph))
  if(is.nan(tc[1])) tc[1] <- choose(vcount(graph), 3) - sum(tc, na.rm = TRUE)
  tc[c(1, 3, 11, 16)]}

# FUNCTION: Triad census for two-mode networks
# REQUIRES NAMED NODES OF BOTH MODES IN ORDER TO MATCH PROJECTION AND ORIGINAL
# OBNOXIOUS WARNING MESSAGES
two.mode.triad.census <- function(bigraph, type = 1, max.wt = Inf) {
  V(bigraph)$name <- V(bigraph)
  if(vcount(bigraph) == 0) return(list(pw = 0,
                                       C = matrix(0, nr = 0, nc = 0),
                                       D = matrix(0, nr = 0, nc = 2)))
  h <- bipartite.projection(bigraph, multiplicity = T)[[1 + type]]
  # list of vees (that are not triangles)
  vl <- do.call(cbind, lapply(V(h)[1:(vcount(h) - 1)], function(v) {
    d2 <- as.numeric(V(h)[
      which(shortest.paths(h, v, (v + 1):vcount(h),
                           weights = NA) == 2) + v
    ])
    gasp <- get.all.shortest.paths(h, v, d2, weights = NA)[[1]]
    do.call(cbind, gasp[sapply(gasp, length) == 3])
  }))
  # list of triangles
  tl <- do.call(cbind, cliques(h, min = 3, max = 3))
  # bound on pair weight
  pw <- if(max.wt == Inf) max(E(h)$weight) else
    if(!is.null(max.wt)) max.wt else
    quantile(E(h)$weight, .95)
  # initialize C (array of triad tallies) and D (array of additional coords)
  C <- as.data.frame(matrix(0, nr = choose(pw + 3, 3), nc = pw + 1))
  D <- matrix(0, nr = 0, nc = 2)
  # for each edge (i, j) add n - |N_1(i) U N_1(j)| to entry ((w_ij, 0, 0), 0)
  pb <- txtProgressBar(min = 1, max = ecount(h), style = 3)
  for(e in 1:ecount(h)) {
    i <- partition.position(c(E(h)[e]$weight, 0, 0)) + 1
    if(E(h)[e]$weight > pw) {
      rep.count <- vcount(h) -
        length(unique(unlist(neighborhood(h, 1, nodes = get.edge(h, e)))))
      stopifnot(rep.count >= 0)
      if(rep.count > 0) D <- rbind(D, t(sapply(1:rep.count,
                                               function(x) c(i + 1, 1))))
    } else {
      C[i, 1] <- C[i, 1] +
        vcount(h) -
        length(unique(unlist(neighborhood(h, 1, nodes = get.edge(h, e)))))
    }
    setTxtProgressBar(pb, e)
  }
  close(pb)
  # for each vee (i, j, k) add 1 to count of ((w_ij, w_jk, 0), 0)
  if(!is.null(vl)) {
    bar <- dim(vl)[2] > 1
    if(bar) pb <- txtProgressBar(min = 1, max = dim(vl)[2], style = 3)
    for(v in 1:dim(vl)[2]) {
      lamb <- sort(c(E(h, P = vl[1:2, v])$weight,
                     E(h, P = vl[2:3, v])$weight), decreasing = TRUE)
      i <- partition.position(c(lamb, 0)) + 1
      if(lamb[1] > pw) { D <- rbind(D, c(i, 1)) } else {
        C[i, 1] <- C[i, 1] + 1
      }
      if(bar) setTxtProgressBar(pb, v)
    }
    if(bar) close(pb)
  }
  # for each triangle (i, j, k) add 1 to count of appropriate triad
  if(!is.null(tl)) {
    bar <- dim(tl)[2] > 1
    if(bar) pb <- txtProgressBar(min = 1, max = dim(tl)[2], style = 3)
    for(t in 1:dim(tl)[2]) {
      tr <- as.numeric(V(h)$name[tl[, t]])
      s <- induced.subgraph(bigraph,
                            vids = unique(unlist(neighborhood(bigraph,
                                                              1, tr))))
      tw <- length(which(degree(s) * (V(s)$type != type) == 3))
      lambda <- sort(c(E(h, P = tl[1:2, t])$weight - tw,
                       E(h, P = tl[2:3, t])$weight - tw,
                       E(h, P = tl[c(1, 3), t])$weight - tw),
                     decreasing = TRUE)
      i <- partition.position(lambda) + 1
      if(lambda[1] > pw || tw > pw) { D <- rbind(D, c(i, 1)) } else {
        C[i, tw + 1] <- C[i, tw + 1] + 1
      }
      if(bar) setTxtProgressBar(pb, t)
    }
    if(bar) close(pb)
  }
  # the rest of the triads share no attributions
  C[1, 1] <- C[1, 1] + choose(vcount(h), 3) - sum(C) - dim(D)[1]
  C <- C[, 1:max(which(colSums(C) > 0))]
  # aggregate D
  if(dim(D)[1] > 0) D <- aggregate(rep(1, dim(D)[1]),
                                   by = list(D[, 1], D[, 2]), 'sum')
  return(list(pw, C, D))
}

# Derive clustering coefficients from two-mode triad census
tc2cc <- function(tc, S.fn, F.fn, num.denom = FALSE) {
  if(dim(tc)[1] * dim(tc)[2] == 0) return(NA)
  S.c <- sum(sapply(1:dim(tc)[2], function(j) sapply(1:dim(tc)[1], function(i) {
    if(tc[i, j] == 0) 0 else
    S.fn(position.partition(3, i - 1), j - 1) * tc[i, j]})))
  F.c <- sum(sapply(1:dim(tc)[2], function(j) sapply(1:dim(tc)[1], function(i) {
    if(tc[i, j] == 0) 0 else
    F.fn(position.partition(3, i - 1), j - 1) * tc[i, j]})))
  return(if(num.denom) c(S.c, S.c + F.c) else S.c / (S.c + F.c))}

# Classical clustering coefficient on the one-mode projection
# tc must be a matrix with rows indexed as partition.position
tc2C <- function(tc, num.denom = FALSE) tc2cc(tc,
  function(L, w) 3 * ((L[3] > 0) | (w > 0)),
  function(L, w) ((L[2] > 0) & (L[3] == 0) & (w == 0)), num.denom = num.denom)

# Global Opsahl clustering coefficient
# (agrees with bipartite.transitivity)
# tc must be a matrix with rows indexed as partition.position
tc2CO <- function(tc, num.denom = FALSE) tc2cc(tc,
  function(L, w) (L[1] * L[2] * (L[3] + w > 0) + L[1] * L[3] + L[2] * L[3] +
    L[1] * w * (L[2] > 0 | w > 1) + L[1] * w * (L[3] > 0 | w > 1) +
    L[2] * w + L[2] * w * (L[3] > 0 | w > 1) +
    2 * L[3] * w +
    2 * choose(w, 2) * max(3 * (w > 2), length(which(L > 0)))),
  function(L, w) (L[1] * L[2] * (L[3] + w == 0) +
    L[1] * (L[2] == 0 & w == 1) + L[1] * (L[3] == 0 & w == 1) +
    L[2] * (L[3] == 0 & w == 1) +
    2 * choose(w, 2) * min(3 * (w == 2), length(which(L == 0)))),
    num.denom = num.denom)

# Global inclusive clustering coefficient
# (existence of not necessarily induced 4-paths and 6-cycles)
# tc must be a matrix with rows indexed as partition.position
tc2Cin <- function(tc, num.denom = FALSE) tc2cc(tc,
  function(L, w) 3 * (length(which(L > 0)) + w > 2),
  function(L, w) (L[2] > 0 & L[3] == 0 & w == 0) +
    2 * (L[1] > 0 & L[2] == 0 & w == 1) +
    3 * (L[1] == 0 & w == 2),
  num.denom = num.denom)

# Global exclusive clustering coefficient
# (existence of induced 4-paths and 6-cycles)
# (agrees with exclusive.transitivity)
# tc must be a matrix with rows indexed as partition.position
tc2Cex <- function(tc, num.denom = FALSE) tc2cc(tc,
  function(L, w) 3 * (L[3] > 0),
  function(L, w) ((L[2] > 0) & (L[3] == 0)), num.denom)

# Pairwise weight–resolved exclusive clustering
# tc must be a matrix with rows indexed as partition.position
tc2Cexij <- function(tc, i, j, num.denom = FALSE) tc2cc(tc,
  function(L, w) 3 * ((L[2] >= i) & (L[3] >= j)),
  function(L, w) ((L[2] >= i) & (L[3] < j)), num.denom)

# Triad weight–resolved exclusive clustering
# tc must be a matrix with rows indexed as partition.position
tc2Cexw <- function(tc, ww, num.denom = FALSE) tc2cc(tc,
  function(L, w) 3 * ((L[3] > 0) & (w == ww)),
  function(L, w) ((L[2] > 0) & (L[3] == 0) & (w == ww)), num.denom)

