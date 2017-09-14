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
#' @param mode Numeric or character; whether to project onto actors (\code{1} or
#'   \code{"actors"}) or onto events (\code{2} or \code{"events"}).
#' @param nv,...,attr Arguments passed to \code{\link[igraph]{add_vertices}}. 
#'   Events added to a dynamic affiliation network should be given time 
#'   attributes.
#' @param affiliations A vector, or list of length \code{nv} of vectors, of 
#'   nodes in \code{graph} of mode \emph{not} \code{mode}, to be linked to the
#'   new node(s).
#' @param actors A vector, or list of length \code{nv} of vectors, of actor 
#'   nodes in \code{graph}, to be linked to the new event(s).
#' @param events A vector, or list of length \code{nv} of vectors, of event 
#'   nodes in \code{graph}, to be linked to the new actor(s).
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
#' @export
add_modes <- function(
  graph, mode = 1,
  nv, ..., attr = list(),
  affiliations = NULL
) {
  stopifnot(is_an(graph))
  if (is.character(mode)) {
    mode <- match.arg(mode, c("actors", "events"))
    mode <- if (mode == "actors") 1 else 2
  } else {
    mode <- as.numeric(mode)
  }
  is_dyn <- (mode == 2) & is_dynamic_an(graph)
  if (is_dyn) {
    if (is.null(list(...)[["time"]]) & is.na(match("time", names(attr)))) {
      warning("No 'time' attribute for new event(s) in a dynamic graph.")
    }
  }
  nvids <- vcount(graph) + 1:nv
  graph <- add_vertices(
    graph = graph,
    nv = nv, ...,
    attr = c(list(type = as.logical(mode - 1)), attr)
  )
  if (!is.null(affiliations)) {
    stopifnot(all(V(graph)[unlist(affiliations)]$type == as.logical(2 - mode)))
    edges_to_add <- if (is.list(affiliations)) {
      stopifnot(length(affiliations) == nv)
      as.vector(do.call(cbind, lapply(1:nv, function(i) rbind(
        nvids[i],
        as.numeric(V(graph)[affiliations[[i]]])
      ))))
    } else {
      as.vector(t(expand.grid(
        nvids,
        as.numeric(V(graph)[affiliations])
      )))
    }
    graph <- add_edges(graph = graph, edges = edges_to_add)
  }
  if (is_dyn) {
    return(as_dynamic_an(graph))
  } else {
    return(as_an(graph))
  }
}

#' @rdname mode_addition
#' @export
add_actors <- function(graph, nv, ..., attr = list(), events = NULL) {
  add_modes(
    graph = graph, mode = 1,
    nv = nv, ..., attr = attr,
    affiliations = events
  )
}

#' @rdname mode_addition
#' @export
add_events <- function(graph, nv, ..., attr = list(), actors = NULL) {
  add_modes(
    graph = graph, mode = 2,
    nv = nv, ..., attr = attr,
    affiliations = actors
  )
}
