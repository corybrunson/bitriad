#' @title Project an affiliation network onto its actors
#'   
#' @description These functions use \code{\link{bipartite_projection}} to 
#'   compute the projections of an affiliation network onto the actor or event 
#'   nodes.
#'   
#' @name mode_projection
#' @param graph An affiliation network.
#' @param mode Numeric or character; whether to project onto actors (\code{1} or
#'   \code{"actors"}) or onto events (\code{2} or \code{"events"}).
#' @param name Character; the attribute of the actor or event nodes in 
#'   \code{graph} to use as names for the nodes in the projection.
#' @examples
#' data(chicago1960s)
#' tab <- table(V(chicago1960s)$type)
#' proj <- actor_projection(chicago1960s)
#' vcount(proj) == tab[1]
#' proj <- event_projection(chicago1960s)
#' vcount(proj) == tab[2]
#' @export
mode_projection <- function(graph, mode = "actors", name = "name") {
  stopifnot(is_an(graph))
  if (is.character(mode)) {
    mode <- match.arg(mode, c("actors", "events"))
    mode <- if (mode == "actors") 1 else 2
  } else {
    mode <- as.numeric(mode)
  }
  if (vcount(graph) == 0) return(make_empty_graph(directed = FALSE))
  V(graph)$name <- if (name == "id") {
    V(graph)
  } else {
    get.vertex.attribute(graph, name)
  }
  bipartite_projection(graph, multiplicity = TRUE)[[mode]]
}

#' @rdname mode_projection
#' @export
actor_projection <- function(graph, name = "name") {
  mode_projection(graph = graph, name = name, mode = 1)
}

#' @rdname mode_projection
#' @export
event_projection <- function(graph, name = "name") {
  mode_projection(graph = graph, name = name, mode = 2)
}

#' @rdname mode_projection
#' @export
actor.projection <- actor_projection

#' @rdname mode_projection
#' @export
event.projection <- event_projection
