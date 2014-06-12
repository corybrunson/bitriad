# bipartite triadic closure (Opsahl 2012)
bipartite.transitivity <- function(
  bigraph, type = 'global', vids = which(V(bigraph)$type == 0),
  status = FALSE
) {
  # Check that nodes are of common type (requires attribute 'type')
  stopifnot(all(V(bigraph)$type[vids] == 0) |
            !any(V(bigraph)$type[vids] == 0))
  # If global, need to look at all vertices
  vs <- if(type != 'local') which(V(bigraph)$type == 0) else vids
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
