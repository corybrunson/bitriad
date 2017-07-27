#' @title Classical triad closure (on projections) for affiliation networks
#'   
#' @description Given an affiliation network and a vector of actor node IDs, 
#'   calculate the classical triad closure (clustering coefficient) centered at 
#'   the actors in the unipartite projection onto the actor nodes.
#'   
#' @details This function recapitulates
#'   \code{\link{triad_closure_watts_strogatz}} by invoking the
#'   \code{\link[igraph]{bipartite_projection}} and
#'   \code{\link[igraph]{transitivity}} functions in \strong{igraph}.
#'   

#' @name triad_closure_projection
#' @family triad closure
#' @param bigraph An affiliation network.
#' @param actors A vector of actor nodes in \code{bigraph}.
#' @param type The type of statistic, matched to \code{"global"}, 
#'   \code{"local"}, or \code{"raw"}.
#' @return If \code{type} is \code{"global"}, the global statistic for 
#'   \code{bigraph}; if \code{"local"}, the local statistics for \code{actors}; 
#'   if \code{"raw"}, a 2-column matrix, each row of which gives the number of
#'   wedges and of closed wedges centered at \code{actors}.
#' @seealso \code{\link{triad_closure_an}}
#' @export
triad_closure_projection <- function(
  bigraph, actors = V(bigraph)[V(bigraph)$type == FALSE],
  type = "global"
) {
  type <- match.arg(type, c("global", "local", "raw"))
  if(vcount(bigraph) == 0) {
    if(type == "global") {
      return(NaN)
    } else if(type == "local") {
      return(NULL)
    } else return(matrix(NA, nrow = 0, ncol = 2))
  }
  stopifnot(all(V(bigraph)$type[actors] == FALSE))
  graph <- actor_projection(bigraph)
  proj_actors <- which(which(!V(bigraph)$type) %in% actors)
  stopifnot(length(proj_actors) == length(actors))
  if(type == "global") {
    return(transitivity(graph, type = "global"))
  }
  C <- transitivity(graph, type = "local", vids = proj_actors)
  if(type == "local") return(C)
  C[is.na(C)] <- 0
  W <- choose(degree(graph)[proj_actors], 2)
  unname(cbind(W, as.integer(W * C)))
}
