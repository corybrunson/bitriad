#' Project an affiliation network onto its events
#' 
#' This function uses the igraph function \code{\link{bipartite.projection}} to
#' compute the projection of an affiliation network onto the event nodes.
#' @param bigraph An affiliation network; see \code{\link{is.an}}.
#' @param name What attribute of the event nodes in bigraph to name the nodes in
#'   the projection (defaults to "name")
#' @examples
#' data(southafrica1905)
#' tab <- table(V(southafrica1905)$type)
#' proj <- event.projection(southafrica1905)
#' vcount(proj) == tab[2]
#' @export

event.projection <-
function(bigraph, name = "name") {
    if(vcount(bigraph) == 0) return(graph.empty())
    if(name == 'id') V(bigraph)$name <- V(bigraph)
    bipartite.projection(bigraph, multiplicity = TRUE)[[2]]
}
