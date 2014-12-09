#' Schedule
#' 
#' The schedule of a subset of actor nodes in an affiliation network is the
#' induced subgraph on the actor nodes together with all event nodes incident
#' to at least two of the actor nodes.
#' @param bigraph The ambient affiliation network
#' @param vids Vertex ids, the event count among which is being considered
#' @param v A subset of actor node ids.
#' @export

share.weight <-
    function(bigraph, vids, name = 'name') {
        if(name == 'id') vids <- as.numeric(vids)
        length(Reduce(intersect, neighborhood(bigraph, 1, as.numeric(vids))))
    }
