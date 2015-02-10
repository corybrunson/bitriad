#' Affiliation network
#' 
#' @param graph A graph object
#' @export

as.an <-
function(graph) {
    # Must be a graph object
    if(!is.igraph(graph)) stop('Not an igraph object')
    # Must have node types (i.e. be "bipartite")
    if(!('type' %in% igraph::list.vertex.attributes(graph)))
        stop('Needs type attribute')
    # There must be no edges between nodes of the same type
    el <- get.edgelist(graph, names = FALSE)
    if(!all(V(graph)$type[el[, 1]] + V(graph)$type[el[, 2]]))
        stop('Type attribute does not form a bipartition')
    # Vertices must be in order of type
    permute.vertices(graph, order(V(graph)$type))
}
