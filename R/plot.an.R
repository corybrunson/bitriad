#' Plot affiliation networks
#' 
#' This function passes affiliation network specifications to `plot.igraph`.
#' @param bigraph An affiliation network.
#' @param layoutFun Layout function to use (defaults to `layout.bipartite`).
#' @param ... Additional parameters passed to layoutFun
#' @export

plot.an <-
    function(bigraph, layoutFun = layout.bipartite, ...) {
        stopifnot(is.an(bigraph))
        # Use number of vertices to scale down vertex size
        vertex.scale <- max(1 / log(vcount(bigraph)), .001)
        # Affiliation network specifications
        V(bigraph)$shape <- ifelse(V(bigraph)$type,
                                   "square", "circle")
        V(bigraph)$size <- ifelse(V(bigraph)$type,
                                  28, 32) * vertex.scale
        V(bigraph)$color <- ifelse(V(bigraph)$type,
                                   "lightcoral", "SkyBlue2")
        V(bigraph)$label.family <- "sans"
        V(bigraph)$label.font <- 2
        V(bigraph)$label.color <- "black"
        V(bigraph)$label.cex <- 2 * vertex.scale
        E(bigraph)$width <- 2
        E(bigraph)$color <- "black"
        # Compute layout
        bigraph$layout <- layoutFun(bigraph, ...)
        # Plot
        plot(bigraph, asp = 0)
    }
