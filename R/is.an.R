#' Affiliation network
#' 
#' @param graph A graph object
#' @export

is.an <-
function(graph) {
    # Must be a graph object
    if(!is.igraph(graph)) return(FALSE)
    # Must have node types (i.e. be "bipartite")
    if(!('type' %in% igraph::list.vertex.attributes(graph))) return(FALSE)
    # Vertices must be in order of type
    if(!all(V(graph)$type == sort(V(graph)$type))) return(FALSE)
    # There must be no edges between nodes of the same type
    el <- get.edgelist(graph, names = FALSE)
    all(V(graph)$type[el[, 1]] + V(graph)$type[el[, 2]])
}
