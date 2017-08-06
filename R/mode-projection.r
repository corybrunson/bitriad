#' @title Project an affiliation network onto its actors
#'   
#' @description These functions use \code{\link{bipartite_projection}} to 
#'   compute the projections of an affiliation network onto the actor or event 
#'   nodes.
#'   
#' @name mode-projection
#' @param bigraph An affiliation network.
#' @param mode Numeric or character; whether to project onto actors (\code{1} or
#'   \code{"actors"}) or onto events (\code{2} or \code{"events"}).
#' @param name Character; the attribute of the actor or event nodes in 
#'   \code{bigraph} to use as names for the nodes in the projection.
#' @examples
#' data(chicago1960s)
#' tab <- table(V(chicago1960s)$type)
#' proj <- actor_projection(chicago1960s)
#' vcount(proj) == tab[1]
#' proj <- event_projection(chicago1960s)
#' vcount(proj) == tab[2]
#' @export
mode_projection <- function(bigraph, mode = "actors", name = "name") {
  stopifnot(is_an(bigraph))
  if (is.character(mode)) {
    mode <- match.arg(mode, c("actors", "events"))
    mode <- if (mode == "actors") 1 else 2
  } else {
    mode <- as.numeric(mode)
  }
  if (vcount(bigraph) == 0) return(make_empty_graph(directed = FALSE))
  V(bigraph)$name <- if (name == "id") {
    V(bigraph)
  } else {
    get.vertex.attribute(bigraph, name)
  }
  bipartite_projection(bigraph, multiplicity = TRUE)[[mode]]
}

#' @rdname mode-projection
#' @export
actor_projection <- function(bigraph, name = "name") {
  mode_projection(bigraph = bigraph, name = name, mode = 1)
}

#' @rdname mode-projection
#' @export
event_projection <- function(bigraph, name = "name") {
  mode_projection(bigraph = bigraph, name = name, mode = 2)
}

#' @rdname mode-projection
#' @export
actor.projection <- actor_projection

#' @rdname mode-projection
#' @export
event.projection <- event_projection
