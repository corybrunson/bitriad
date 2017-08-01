#' @title Actors and their shared events
#'   
#' @description Given an affiliation network and a vector of actor node IDs, 
#'   produce the induced subgraph on the actor nodes together with all event 
#'   nodes incident to at least two of them. This is called the actors'
#'   *schedule*.
#'   
#' @name schedule
#' @param bigraph An affiliation network.
#' @param actors A vector of actor nodes in \code{bigraph}.
#' @export
schedule <- function(
  bigraph,
  actors = V(bigraph)[V(bigraph)$type == FALSE]
) {
  stopifnot(all(V(bigraph)$type[actors] == FALSE))
  actors <- as.numeric(V(bigraph)[actors])
  events_table <- table(unlist(neighborhood(bigraph, 1, actors)))
  coattended <- as.numeric(names(events_table)[events_table > 1])
  induced_subgraph(bigraph, c(actors, coattended))
}
