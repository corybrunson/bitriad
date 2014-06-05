# FUNCTION: Triad census for two-mode networks
# REQUIRES NAMED NODES OF BOTH MODES IN ORDER TO MATCH PROJECTION AND ORIGINAL
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

