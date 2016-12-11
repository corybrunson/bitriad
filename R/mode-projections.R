#' Project an affiliation network onto its actors
#' 
#' These functions use the igraph function \code{\link{bipartite_projection}} to
#' compute the projections of an affiliation network onto the actor and event
#' nodes.
#' 
#' @name mode_projections
#' @param bigraph An affiliation network; see \code{\link{is_an}}.
#' @param name What attribute of the actor nodes in bigraph to name the nodes in
#'   the projection (defaults to "name")
#' @examples
#' data(southafrica1905)
#' tab <- table(V(southafrica1905)$type)
#' proj <- actor_projection(southafrica1905)
#' vcount(proj) == tab[1]
#' proj <- event_projection(southafrica1905)
#' vcount(proj) == tab[2]
#' @export
actor_projection <-
  function(bigraph, name = "name") {
    if(vcount(bigraph) == 0) return(make_empty_graph(directed = FALSE))
    if(name == "id") V(bigraph)$name <- V(bigraph)
    bipartite_projection(bigraph, multiplicity = TRUE)[[1]]
  }

#' @rdname mode_projections
#' @export
event_projection <-
  function(bigraph, name = "name") {
    if(vcount(bigraph) == 0) return(make_empty_graph(directed = FALSE))
    if(name == "id") V(bigraph)$name <- V(bigraph)
    bipartite_projection(bigraph, multiplicity = TRUE)[[2]]
  }

#' @rdname mode_projections
#' @export
actor.projection <- actor_projection

#' @rdname mode_projections
#' @export
event.projection <- event_projection
