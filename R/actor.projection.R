#' Projection onto actors of an affiliation network
#'
#' This function uses the igraph function bipartite.projection to compute the
#' projection of an affiliation network onto the actor nodes.
#' @param bigraph The ambient affiliation network
#' @param name What attribute of the actor nodes in bigraph to name the nodes
#' in the projection (defaults to 'name')
#' @export
#' @examples
#' data(hobson.inner.circle)
#' tab <- table(V(hobson.inner.circle)$type)
#' proj <- actor.projection(hobson.inner.circle)
#' vcount(proj) == tab[1]

actor.projection <-
function(bigraph, name = 'name') {
    if(vcount(bigraph) == 0) return(graph.empty())
    if(name == 'id') V(bigraph)$name <- V(bigraph)
    bipartite.projection(bigraph, multiplicity = TRUE)[[1]]
}
