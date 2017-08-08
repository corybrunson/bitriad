#' @title Dynamic graphs
#'   
#' @description An affiliation network is \emph{dynamic}, for present purposes, 
#'   if its event nodes have time stamps, recorded as a numeric vertex attribute
#'   \code{"time"}.
#'   
#' @name dynamic_an
#' @param graph An \code{igraph} object.
#' @export
is_dynamic_an <- function(graph) {
  if (!is_igraph(graph)) stop("Not a graph object.")
  if (!is_an(graph)) stop("Not an affiliation network.")
  if (!("time" %in% vertex_attr_names(graph))) return(FALSE)
  is.numeric(vertex_attr(
    graph = graph,
    name = "time",
    index = V(graph)[V(graph)$type == TRUE]
  ))
}

#' @rdname dynamic_an
#' @export
is.dyn <- function(graph) {
  .Deprecated("is_dynamic_an")
  is_dynamic_an(graph)
}
