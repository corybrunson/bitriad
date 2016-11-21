#' Count the events in an affiliation network
#'
#' This function returns the number of events (nodes of type TRUE) in an
#' affiliation network.
#' @param bigraph An affiliation network; see \code{\link{is_an}}.
#' @examples
#' data(southafrica1905)
#' event_count(southafrica1905)
#' @export

event_count <-
  function(bigraph) {
    if(vcount(bigraph) == 0) return(0)
    if(!is_an(bigraph)) stop('Not an affiliation network')
    length(which(V(bigraph)$type))
  }
