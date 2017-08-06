#' @title Take the dual of an affiliation network
#'   
#' @description This function obtains the dual of an affiliation network, in
#'   which the actors and events have swapped roles. To do this, it negates
#'   the logical values of the node \code{type} attribute and reorders the
#'   node ids accordingly.
#'   
#' @name dualize
#' @param bigraph An affiliation network.
#' @import igraph
#' @examples
#' data(women_clique)
#' tab <- table(V(women_clique)$type)
#' proj <- actor_projection(dualize(women_clique))
#' vcount(proj) == tab[2]
#' @export
dualize <- function(bigraph) {
  stopifnot(is_an(bigraph))
  V(bigraph)$type <- !V(bigraph)$type
  permute(bigraph, order(order(V(bigraph)$type)))
}

#' @rdname dualize
#' @export
dual_an <- function(bigraph) {
  .Deprecated("dualize")
  dualize(bigraph)
}

#' @rdname dualize
#' @export
dual.an <- function(bigraph) {
  .Deprecated("dualize")
  dualize(bigraph)
}
