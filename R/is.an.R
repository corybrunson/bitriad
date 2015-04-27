#' Test and coerce affiliation network conditions
#' 
#' These functions test for or, when possible, impose the properties used in
#' bitriad to distinguish affiliation networks from the very general igraph
#' class.
#' @param graph An igraph object
#' @export

is.an <-
function(graph) {
    # Must be a graph object
    if(!is.igraph(graph)) return(FALSE)
    if(vcount(graph) == 0) return(TRUE)
    # Must have node types (i.e. be "bipartite")
    if(!('type' %in% igraph::list.vertex.attributes(graph))) return(FALSE)
    # Vertices must be in order of type
    if(!all(V(graph)$type == sort(V(graph)$type))) return(FALSE)
    # There must be no edges between nodes of the same type
    el <- get.edgelist(graph, names = FALSE)
    all(V(graph)$type[el[, 1]] + V(graph)$type[el[, 2]])
}
