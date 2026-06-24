#' @title Actors and their shared events
#'   
#' @description Given an affiliation network and a vector of actor node IDs, 
#'   produce the induced subgraph on the actor nodes together with all event 
#'   nodes incident to at least two of them. This is called the actors'
#'   *schedule*.
#'   
#' @name schedule
#' @family modal queries and manipulations
#' @param graph An affiliation network.
#' @param actors A vector of actor nodes in `graph`.
#' @returns An `igraph` object induced from the input `graph`.
#' @examples
#' data(women_clique)
#' schedule(women_clique, actors = V1(women_clique)[seq(3)])
#' @export
schedule <- function(
  graph,
  actors = V(graph)[V(graph)$type == FALSE]
) {
  stopifnot(all(V(graph)$type[actors] == FALSE))
  actors <- as.numeric(V(graph)[actors])
  events_table <- table(unlist(lapply(ego(graph, 1, actors), as_ids)))
  coattended <- match(names(events_table)[events_table > 1], V2(graph)$name)
  induced_subgraph(graph, c(V1(graph)[actors], V2(graph)[coattended]))
}
