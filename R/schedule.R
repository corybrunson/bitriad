#' Schedule
#'
#' The schedule of a subset of actor nodes in an affiliation network is the
#' induced subgraph on the actor nodes together with all event nodes incident
#' to at least two of the actor nodes.
#' 
#' @name schedule
#' @param bigraph The ambient affiliation network
#' @param v A subset of actor node ids.
#' @param graph A one-mode network (used as the one-mode projection of bigraph)
#' @param vids Vertex ids, the event count among which is being considered
#' @param vp Ids of vertex pair, the edge between which is being considered
#' @export
schedule <-
  function(bigraph, v) {
    stopifnot(all(!V(bigraph)$type[v]))
    events <- unlist(neighborhood(bigraph, 1, v))
    tab <- table(events)
    coattended <- as.numeric(names(tab)[tab > 1])
    induced_subgraph(bigraph, c(v, coattended))
  }

#' @rdname schedule
edgeWeight <-
  function(graph, vp) {
    id <- get.edge.ids(graph, vp)
    if(id == 0) 0 else E(graph)$weight[id]
  }

#' @rdname schedule
shareWeight <-
  function(bigraph, vids) {
    length(Reduce(intersect, neighborhood(bigraph, 1, as.numeric(vids))))
  }
