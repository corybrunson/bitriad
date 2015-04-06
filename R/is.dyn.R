#' Dynamic graphs
#' 
#' A simple graph is dynamic if its edges have time stamps. A bipartite graph is
#' dynamic if its nodes (most importantly, its event nodes) have time stamps.
#' @param graph An igraph object
#' @export

is.dyn <-
    function(graph) {
        if (!is.igraph(graph)) {
            stop("Not a graph object")
        }
        if(is.bipartite(graph)) {
            "time" %in% igraph::list.vertex.attributes(graph)
        } else {
            "time" %in% igraph::list.edge.attributes(graph)
        }
    }
