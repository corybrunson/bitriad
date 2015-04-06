#' Dual of an affiliation network
#'
#' This function swaps the TRUE and FALSE values of the `type` attribute,
#' which in `bitriad` has the effect of taking the dual of the affiliation
#' network (swapping the roles of actors and events).
#' @param bigraph The ambient affiliation network
#' @export
#' @examples
#' data(inner.circle)
#' tab <- table(V(inner.circle)$type)
#' proj <- actor.projection(dual.an(inner.circle))
#' vcount(proj) == tab[2]

dual.an <-
function(bigraph) {
    if(!is.an(bigraph)) stop('Not an affiliation network')
    V(bigraph)$type <- !V(bigraph)$type
    bigraph
}
