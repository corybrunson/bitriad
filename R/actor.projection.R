#' Project an affiliation network onto its actors
#' 
#' This function uses the igraph function \code{\link{bipartite.projection}} to
#' compute the projection of an affiliation network onto the actor nodes.
#' @param bigraph An affiliation network; see \code{\link{is.an}}.
#' @param name What attribute of the actor nodes in bigraph to name the nodes in
#'   the projection (defaults to "name")
#' @examples
#' data(southafrica1905)
#' tab <- table(V(southafrica1905)$type)
#' proj <- actor.projection(southafrica1905)
#' vcount(proj) == tab[1]
#' @export

actor.projection <-
function(bigraph, name = "name") {
    if(vcount(bigraph) == 0) return(graph.empty())
    if(name == 'id') V(bigraph)$name <- V(bigraph)
    bipartite.projection(bigraph, multiplicity = TRUE)[[1]]
}
