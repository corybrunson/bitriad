#' Count the events in an affiliation network
#'
#' This function returns the number of events (nodes of type TRUE) in an
#' affiliation network.
#' @param bigraph An affiliation network; see \code{\link{is.an}}.
#' @examples
#' data(southafrica1905)
#' event.count(southafrica1905)
#' @export

event.count <-
function(bigraph) {
    if(vcount(bigraph) == 0) return(0)
    if(!is.an(bigraph)) stop('Not an affiliation network')
    length(which(V(bigraph)$type))
}
