
edgeWeight <- function(graph, vp) {
  id <- get.edge.ids(graph, vp)
  if(id == 0) 0 else E(graph)$weight[id]
}

shareWeight <- function(bigraph, vids) {
  length(Reduce(intersect, neighborhood(bigraph, 1, as.numeric(vids))))
}
