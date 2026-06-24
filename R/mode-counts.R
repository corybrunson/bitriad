#' @title Count the actors and events in an affiliation network
#'
#' @description These functions return the number of actors (nodes with `type`
#'   attribute `FALSE`) or events (`TRUE`) in an affiliation network.
#'
#' @name mode_counts
#' @family modal queries and manipulations
#' @seealso Original **igraph** functions: [vcount()], [ecount()]
#' @param graph An affiliation network.
#' @returns An integer.
#' @examples
#' data(chicago1960s)
#' actor_count(chicago1960s)
#' event_count(chicago1960s)
NULL

#' @rdname mode_counts
#' @export
actor_count <- function(graph) {
  stopifnot(is_an(graph))
  if (vcount(graph) == 0) return(0)
  length(which(!V(graph)$type))
}

#' @rdname mode_counts
#' @export
event_count <- function(graph) {
  stopifnot(is_an(graph))
  if (vcount(graph) == 0) return(0)
  length(which(V(graph)$type))
}

#' @rdname mode_counts
#' @export
actor.count <- actor_count

#' @rdname mode_counts
#' @export
event.count <- event_count
