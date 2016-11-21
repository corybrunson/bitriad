#' Project an affiliation network onto its events
#' 
#' This function uses the igraph function \code{\link{bipartite_projection}} to
#' compute the projection of an affiliation network onto the event nodes.
#' 
#' @import igraph
#' @param bigraph An affiliation network; see \code{\link{is_an}}.
#' @param name What attribute of the event nodes in bigraph to name the nodes in
#'   the projection (defaults to "name")
#' @examples
#' data(southafrica1905)
#' tab <- table(igraph::V(southafrica1905)$type)
#' proj <- event_projection(southafrica1905)
#' igraph::vcount(proj) == tab[2]
#' @export

event_projection <-
  function(bigraph, name = "name") {
    if(vcount(bigraph) == 0) return(make_empty_graph(directed = FALSE))
    if(name == "id") V(bigraph)$name <- V(bigraph)
    bipartite_projection(bigraph, multiplicity = TRUE)[[2]]
  }
