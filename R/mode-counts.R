#' @title Count the actors and events in an affiliation network
#'   
#' @description These functions return the number of actors (nodes with
#'   \code{type} attribute \code{FALSE}) or events (\code{TRUE}) in an
#'   affiliation network.
#'   
#' @name mode_counts
#' @param bigraph An affiliation network.
#' @examples
#' data(chicago1960s)
#' actor_count(chicago1960s)
#' event_count(chicago1960s)
NULL

#' @rdname mode_counts
#' @export
actor_count <- function(bigraph) {
  stopifnot(is_an(bigraph))
  if (vcount(bigraph) == 0) return(0)
  length(which(!V(bigraph)$type))
}

#' @rdname mode_counts
#' @export
event_count <- function(bigraph) {
  stopifnot(is_an(bigraph))
  if (vcount(bigraph) == 0) return(0)
  length(which(V(bigraph)$type))
}

#' @rdname mode_counts
#' @export
actor.count <- actor_count

#' @rdname mode_counts
#' @export
event.count <- event_count
