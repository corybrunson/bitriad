#' Set plotting parameters for an affiliation network
#' 
#' This function assigns several plotting parameters to the vertices and edges
#' of a bipartite graph to prepare it for visualization using `plot.igraph`.
#' @param bigraph An affiliation network; see `is.an`.
#' @export
#' @examples
#' data(women.clique)
#' data(whigs)
#' for(g in list(women.clique, whigs)) {
#'     plot(prettify.an(g))
#' }

prettify.an <-
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
        E(bigraph)$width <- .5 + 2.5 * vertex.scale
        # Return graph
        bigraph
    }
