#' Projection onto actors of an affiliation network
#' 
#' This function uses the igraph function bipartite.projection to compute the
#' projection of an affiliation network onto the actor nodes.
#' @param bigraph The ambient affiliation network
#' @param type The node type (0 or 1) of the actors of bigraph
#' @param name What attribute of the actor nodes in bigraph to name the nodes
#' in the projection (defaults to 'name')
#' @export

actor.projection <-
function(bigraph, type = 0, name = 'name') {
    if(name == 'id') V(bigraph)$name <- V(bigraph)
    return(bipartite.projection(bigraph, multiplicity = TRUE)[[1 + type]])
}
