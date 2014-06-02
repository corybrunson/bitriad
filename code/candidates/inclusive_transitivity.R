# Inclusive clustering (Brunson)
# (first candidate clustering coefficient)
# (existence of not necessarily induced 4-paths and 6-cycles)
# (does not satisfy requirements laid out in abstract)
inclusive.transitivity <- function(
  bigraph, type = 'global', vids = V(bigraph)[V(bigraph)$type == 1]
) {
  # Check that nodes are of common type (requires attribute 'type')
  stopifnot(all(V(bigraph)$type[vids] == 1) | !any(V(bigraph)$type[vids] == 1))
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
    # tallied only for vertices of given type though histogram ranges over all
    l4 <- hist(L4[2, ], breaks = 0:vcount(bigraph) + .5)$counts
    c6 <- hist(L4[2, L4[4, ] == 1], breaks = 0:vcount(bigraph) + .5)$counts
  }
  if(type == 'global') return(nC) else
  if(type == 'local') return(c6[vids] / l4[vids]) else
  return(list(stratified = data.frame(l4 = l4[vids], c6 = c6[vids]),
              global = nC))
}

