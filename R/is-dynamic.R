#' Dynamic graphs
#' 
#' A simple graph is dynamic if its edges have time stamps. A bipartite graph is
#' dynamic if its event nodes have time stamps.
#' 
#' @param graph An igraph object.
#' @export
is_dynamic <-
  function(graph) {
    if (!is_igraph(graph)) {
      stop("Not a graph object")
    }
    if(is_bipartite(graph)) {
      "time" %in% vertex_attr_names(graph)
    } else {
      "time" %in% vertex_attr_names(graph)
    }
  }
