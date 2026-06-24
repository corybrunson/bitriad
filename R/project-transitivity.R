#' Affiliation network clustering coefficients
#'
#' Each clustering coefficient can be defined as the proportion of "wedges" that
#' are "closed", for suitable definitions of both terms. The main function,
#' [transitivity_an()], calls one of the wedge functions and computes the global
#' or local clustering coefficient of the given affiliation network, and if the
#' local, then at the given nodes. (`project_transitivity` cheats by using
#' [transitivity()] but produces output consistent with the other
#' variants of `transitivity_an`.)
#'
#' @name project_transitivity
#' @family triad closure functions
#' @family clustering coefficients
#' @param graph An affiliation network.
#' @param type The type of clustering coefficient (defaults to "global")
#' @param vids A subset of actor node ids at which to evaluate the local
#'   clustering coefficient.
#' @returns If `type` is `"global"`, the global clustering coefficient of the
#'   network (a single numeric value); if `"local"`, the local clustering
#'   coefficients of the actors (a numeric vector); otherwise, a 2-column
#'   matrix, each row of which gives the number of wedges and the number of
#'   closed wedges centered at each actor.
#' @export
project_transitivity <- function(
  graph, type = "global",
  vids = which(!V(graph)$type)
) {
  .Deprecated("triad_closure_projection")
  if(vcount(graph) == 0) {
    if(type == 'global') {
      return(NaN)
    } else if(type == 'local') {
      return(NULL)
    } else return(matrix(NA, nrow = 0, ncol = 2))
  }
  stopifnot(all(!V(graph)$type[vids]))
  proj <- actor_projection(graph)
  proj_vids <- which(which(!V(graph)$type) %in% vids)
  stopifnot(length(proj_vids) == length(vids))
  if(type == 'global') {
    return(transitivity(proj, type = 'global'))
  }
  C <- transitivity(proj, type = 'local', vids = proj_vids)
  if(type == 'local') return(C)
  C[is.na(C)] <- 0
  W <- choose(degree(proj)[proj_vids], 2)
  unname(cbind(W, W * C))
}

#' @rdname project_transitivity
#' @export
project.transitivity <- project_transitivity
