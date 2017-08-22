#' @title Actors and their shared events
#'   
#' @description Given an affiliation network and a vector of actor node IDs, 
#'   produce the induced subgraph on the actor nodes together with all event 
#'   nodes incident to at least two of them. This is called the actors'
#'   \emph{schedule}.
#'   
#' @name schedule
#' @param graph An affiliation network.
#' @param actors A vector of actor nodes in \code{graph}.
#' @export
schedule <- function(
  graph,
  actors = V(graph)[V(graph)$type == FALSE]
) {
  stopifnot(all(V(graph)$type[actors] == FALSE))
  actors <- as.numeric(V(graph)[actors])
  events_table <- table(unlist(neighborhood(graph, 1, actors)))
  coattended <- as.numeric(names(events_table)[events_table > 1])
  induced_subgraph(graph, c(actors, coattended))
}
