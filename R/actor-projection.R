#' Project an affiliation network onto its actors
#' 
#' This function uses the igraph function \code{\link{bipartite_projection}} to
#' compute the projection of an affiliation network onto the actor nodes.
#' 
#' @param bigraph An affiliation network; see \code{\link{is_an}}.
#' @param name What attribute of the actor nodes in bigraph to name the nodes in
#'   the projection (defaults to "name")
#' @examples
#' data(southafrica1905)
#' tab <- table(igraph::V(southafrica1905)$type)
#' proj <- actor_projection(southafrica1905)
#' igraph::vcount(proj) == tab[1]
#' @export
actor_projection <-
  function(bigraph, name = "name") {
    if(vcount(bigraph) == 0) return(make_empty_graph(directed = FALSE))
    if(name == "id") V(bigraph)$name <- V(bigraph)
    bipartite_projection(bigraph, multiplicity = TRUE)[[1]]
  }
