#' One-mode (actor) projection of a two-mode (affiliation) network
#' 
#' This function uses the igraph function bipartite.projection to compute the
#' projection of a two-mode network onto the desired node type.
#' @param bigraph The ambient affiliation network
#' @param type The actor node type in bigraph
#' @param name What attribute of the actor nodes in bigraph to name the nodes
#' in the projection (defaults to 'name')
#' @export

onemode.projection <-
function(bigraph, type = 0, name = 'name') {
    if(name == 'id') V(bigraph)$name <- V(bigraph)
    return(bipartite.projection(bigraph, multiplicity = TRUE)[[1 + type]])
}
