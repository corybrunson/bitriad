# exclusive clustering (Brunson)
Exclusive.transitivity <- function(
  bigraph, type = 'global', vids = V(bigraph)[V(bigraph)$type == 0]
) {
  # Check that nodes are of common type (requires attribute 'type')
  stopifnot(all(V(bigraph)$type[vids] == 0) | !any(V(bigraph)$type[vids] == 0))
  # If global, need to look at all vertices
  vs <- if(type != 'local') V(bigraph)[V(bigraph)$type == 0] else vids
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

