#' Number of events in an affiliation network
#'
#' This function returns the number of events (nodes of type TRUE) in an
#' affiliation network.
#' @param bigraph The ambient affiliation network
#' @export
#' @examples
#' data(hobson.inner.circle)
#' event.count(hobson.inner.circle)

event.count <-
function(bigraph) {
    if(!is.an(bigraph)) stop('Not an affiliation network')
    length(which(V(bigraph)$type))
}
