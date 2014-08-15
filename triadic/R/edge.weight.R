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

edge.weight <-
function(graph, vp) {
        id <- get.edge.ids(graph, vp)
        if(id == 0) 0 else E(graph)$weight[id]
    }
