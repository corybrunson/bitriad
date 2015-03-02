#' Dual of an affiliation network
#'
#' This function swaps the TRUE and FALSE values of the `type` attribute,
#' which in `bitriad` has the effect of taking the dual of the affiliation
#' network (swapping the roles of actors and events).
#' @param bigraph The ambient affiliation network
#' @export
#' @examples
#' data(hobson.inner.circle)
#' tab <- table(V(hobson.inner.circle)$type)
#' proj <- actor.projection(an.dual(hobson.inner.circle))
#' vcount(proj) == tab[2]

an.dual <-
function(bigraph) {
    if(!is.an(bigraph)) stop('Not an affiliation network')
    V(bigraph)$type <- !V(bigraph)$type
    bigraph
}
