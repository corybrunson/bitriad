#' Affiliation network clustering coefficients
#'
#' Each clustering coefficient can be defined as the proportion of "wedges" that
#' are "closed", for suitable definitions of both terms. The main function,
#' transitivity_an, calls one of the wedge functions and computes the
#' global or local clustering coefficient of the given affiliation network,
#' and if the local, then at the given nodes. (`project_transitivity`
#' cheats by using the native `transitivity` but produces output consistent
#' with the other variants of `transitivity_an`.)
#' 
#' @name project_transitivity
#' @param bigraph An affiliation network.
#' @param type The type of clustering coefficient (defaults to "global")
#' @param vids A subset of actor node ids at which to evaluate the local
#' clustering coefficient.
#' @export
#' @family clustering coefficients
#' @seealso \code{\link{opsahl_transitivity}}, \code{\link{opsahl_transitivity}}
project_transitivity <- function(
  bigraph, type = "global",
  vids = which(!V(bigraph)$type)
) {
  .Deprecated("triad_closure_projection")
  if(vcount(bigraph) == 0) {
    if(type == 'global') {
      return(NaN)
    } else if(type == 'local') {
      return(NULL)
    } else return(matrix(NA, nrow = 0, ncol = 2))
  }
  stopifnot(all(!V(bigraph)$type[vids]))
  graph <- actor_projection(bigraph)
  proj_vids <- which(which(!V(bigraph)$type) %in% vids)
  stopifnot(length(proj_vids) == length(vids))
  if(type == 'global') {
    return(transitivity(graph, type = 'global'))
  }
  C <- transitivity(graph, type = 'local', vids = proj_vids)
  if(type == 'local') return(C)
  C[is.na(C)] <- 0
  W <- choose(degree(graph)[proj_vids], 2)
  unname(cbind(W, W * C))
}

#' @rdname project_transitivity
#' @export
project.transitivity <- project_transitivity
