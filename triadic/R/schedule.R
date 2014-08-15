#' Schedule
#' 
#' The schedule of a subset of actor nodes in an affiliation network is the
#' induced subgraph on the actor nodes together with all event nodes incident
#' to at least two of the actor nodes.
#' @param bigraph The ambient affiliation network
#' @param graph A one-mode network (used as the one-mode projection of bigraph)
#' @param vids Vertex ids, the event count among which is being considered
#' @param vp Ids of vertex pair, the edge between which is being considered
#' @param v A subset of actor node ids.
#' @export

schedule <-
function(bigraph, v) {
    stopifnot(all(V(bigraph)$type[v] == 0))
    events <- unlist(neighborhood(bigraph, 1, v))
    tab <- table(events)
    coattended <- as.numeric(names(tab)[tab > 1])
    return(induced.subgraph(bigraph, c(v, coattended)))
}
