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

