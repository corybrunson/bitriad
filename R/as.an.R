#' Affiliation network
#'
#' An affiliation network is, essentially, a bipartite graph. The igraph
#' function `is.bipartite` tests an igraph object for the logical vertex
#' attribute `type` (if it has any vertices at all), which is intended to
#' provide a bipartition of the vertices. It does not, however, test whether
#' edges exist within either part of the partition. The function `is.an` tests
#' this condition. To simplify some functions, affiliation networks, as defined
#' here, must also have their vertices indexed in order of part; all actor
#' vertices must precede all event vertices. The function `is.an` tests also for
#' this condition. The coersive function `as.an` imposes, by a minimal
#' permutation of vertex labels, provided the `igraph` object is bipartite to
#' begin with.
#' @param graph An igraph object
#' @export

as.an <-
function(graph) {
    # Must be a graph object
    if(!is.igraph(graph)) stop('Not an igraph object')
    # Trivial graphs are OK
    if(vcount(graph) == 0) return(graph)
    # Must have node types (i.e. be "bipartite")
    if(!('type' %in% igraph::list.vertex.attributes(graph))) {
        
        # REPLACE THIS WITH A SCRIPT TO START AT THE FIRST NODE WITH type 0,
        # THEN ASSIGN TYPES ACCORDING TO SHORTEST PATH LENGTH FROM FIRST NODE
        stop('Needs type attribute')
    }
    # There must be no edges between nodes of the same type
    el <- get.edgelist(graph, names = FALSE)
    if(!all(V(graph)$type[el[, 1]] + V(graph)$type[el[, 2]]))
        stop('Type attribute does not form a bipartition')
    # Put nodes in order of type
    ord <- order(order(V(graph)$type))
    permute.vertices(graph, ord)
}
