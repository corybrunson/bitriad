#' Parameter specs for plotting affiliation networks
#' 
#' This function assigns several parameters to a bipartite graph to prepare it
#' for visualization using `plot.igraph`.
#' @param bigraph An affiliation network.
#' @export

anPlotSpecs <-
    function(bigraph) {
        stopifnot(is.an(bigraph))
        # Use number of vertices to scale down vertex size
        vertex.scale <- max(1 / log(vcount(bigraph)), .001)
        # Vertex style specs
        V(bigraph)$shape <- ifelse(V(bigraph)$type,
                                   "square", "circle")
        V(bigraph)$size <- ifelse(V(bigraph)$type,
                                  28, 32) * vertex.scale
        V(bigraph)$color <- ifelse(V(bigraph)$type,
                                   "lightcoral", "SkyBlue2")
        # Labeling specs
        V(bigraph)$label.family <- "sans"
        V(bigraph)$label.font <- 2
        V(bigraph)$label.color <- "black"
        V(bigraph)$label.cex <- 2 * vertex.scale
        # Edge specs
        E(bigraph)$width <- 2
        # Return graph
        bigraph
    }
