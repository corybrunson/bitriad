#' Dynamic graphs
#' 
#' A simple graph is dynamic if its edges have time stamps. A bipartite graph is
#' dynamic if its event nodes have time stamps.
#' 
#' @param graph An igraph object.
#' @export
is_dynamic_an <-
  function(graph) {
    if (!is_igraph(graph)) stop("Not a graph object")
    if (!is_an(graph)) stop("Not an affiliation network")
    "time" %in% vertex_attr_names(graph)
  }
