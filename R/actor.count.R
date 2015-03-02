#' Number of actors in an affiliation network
#'
#' This function returns the number of actors (nodes of type FALSE) in an
#' affiliation network.
#' @param bigraph The ambient affiliation network
#' @export
#' @examples
#' data(hobson.inner.circle)
#' actor.count(hobson.inner.circle)

actor.count <-
function(bigraph) {
    if(!is.an(bigraph)) stop('Not an affiliation network')
    length(which(!V(bigraph)$type))
}
