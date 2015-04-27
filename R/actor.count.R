#' Count the actors in an affiliation network
#'
#' This function returns the number of actors (nodes of type FALSE) in an
#' affiliation network.
#' @param bigraph An affiliation network; see `is.an`.
#' @export
#' @examples
#' data(southafrica1905)
#' actor.count(southafrica1905)

actor.count <-
function(bigraph) {
    if(vcount(bigraph) == 0) return(0)
    if(!is.an(bigraph)) stop('Not an affiliation network')
    length(which(!V(bigraph)$type))
}
