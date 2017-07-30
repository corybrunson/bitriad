#' Count the actors and events in an affiliation network
#' 
#' These functions return the number of actors (nodes of type "FALSE") or events
#' ("TRUE") in an affiliation network.
#' 
#' @name mode_counts
#' @param bigraph An affiliation network; see \code{\link{is_an}}.
#' @examples
#' data(chicago1960s)
#' actor_count(chicago1960s)
#' event_count(chicago1960s)
#' @export
actor_count <-
  function(bigraph) {
    if(vcount(bigraph) == 0) return(0)
    if(!is_an(bigraph)) stop('Not an affiliation network')
    length(which(!V(bigraph)$type))
  }

#' @rdname mode_counts
#' @export
event_count <-
  function(bigraph) {
    if(vcount(bigraph) == 0) return(0)
    if(!is_an(bigraph)) stop('Not an affiliation network')
    length(which(V(bigraph)$type))
  }

#' @rdname mode_counts
#' @export
actor.count <- actor_count

#' @rdname mode_counts
#' @export
event.count <- event_count
