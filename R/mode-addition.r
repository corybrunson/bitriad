#' @title Add actor and event nodes
#'   
#' @description These functions add actor and event nodes (as desired) to a 
#'   graph while maintaining its (temporal) affiliation network structure.
#'   
#' @name mode_addition
#' @family modal queries and manipulations
#' @seealso Original \strong{igraph} functions:
#'   \code{\link[igraph]{add_vertices}}, \code{\link[igraph]{add_edges}}
#' @param graph An affiliation network.
#' @param nv,...,attr Arguments passed to \code{\link[igraph]{add_vertices}}. 
#'   Events added to a dynamic affiliation network should be given time 
#'   attributes.
#' @param actors A vector of actor nodes in \code{graph}, to be linked to the 
#'   new event(s).
#' @param events A vector of event nodes in \code{graph}, to be linked to the 
#'   new actor(s).
#' @examples
#' data(women_clique)
#' plot(prettify_an(add_actors(women_clique, nv = 1, events = c(7, 9))))
#' data(women_group)
#' plot(prettify_an(women_group))
#' actor_names <- c("Frances", "Dorothy")
#' cbind(
#'   dynamic_triad_closure(women_group, type = "local"),
#'   dynamic_triad_closure(
#'     add_events(women_group, nv = 1, actors = actor_names, time = 0),
#'     type = "local"
#'   ),
#'   dynamic_triad_closure(
#'     add_events(women_group, nv = 1, actors = actor_names, time = 367),
#'     type = "local"
#'   )
#' )
NULL

#' @rdname mode_addition
#' @export
add_actors <- function(graph, nv, ..., attr = list(), events = NULL) {
  nvids <- vcount(graph) + 1:nv
  graph <- add_vertices(graph = graph,
                        nv = nv, ..., attr = c(list(type = FALSE), attr))
  if (!is.null(events)) {
    graph <- add_edges(graph = graph, edges = as.vector(t(expand.grid(
      nvids,
      as.numeric(V(graph)[events])
    ))))
  }
  as_an(graph)
}

#' @rdname mode_addition
#' @export
add_events <- function(graph, nv, ..., attr = list(), actors = NULL) {
  nvids <- vcount(graph) + 1:nv
  is_dyn <- is_dynamic_an(graph)
  if (is_dyn) {
    if (is.null(list(...)[["time"]]) & is.na(match("time", names(attr)))) {
      warning("No 'time' attribute for new event(s) in a dynamic graph.")
    }
  }
  graph <- add_vertices(graph = graph,
                        nv = nv, ..., attr = c(list(type = TRUE), attr))
  if (!is.null(actors)) {
    graph <- add_edges(graph = graph, edges = as.vector(t(expand.grid(
      as.numeric(V(graph)[actors]),
      nvids
    ))))
  }
  if (is_dyn) {
    return(as_dynamic_an(graph))
  } else {
    return(as_an(graph))
  }
}
