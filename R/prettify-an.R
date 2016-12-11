#' Set plotting parameters for an affiliation network
#' 
#' This function assigns several plotting parameters to the vertices and edges 
#' of a bipartite graph to prepare it for visualization using
#' \code{plot.igraph}.
#' 
#' @name prettify
#' @param bigraph An affiliation network; see \code{is_an}.
#' @export
#' @examples
#' library(igraph)
#' data(women_clique)
#' data(whigs)
#' for(g in list(women_clique, whigs)) {
#'     plot(prettify_an(g))
#' }
prettify_an <-
  function(bigraph) {
    stopifnot(is_an(bigraph))
    # Use number of vertices to scale down vertex size
    vertex_scale <- max(1 / log(vcount(bigraph)), .001)
    # Vertex style specs
    V(bigraph)$shape <- ifelse(V(bigraph)$type,
                               "square", "circle")
    V(bigraph)$size <- ifelse(V(bigraph)$type,
                              28, 32) * vertex_scale
    V(bigraph)$color <- ifelse(V(bigraph)$type,
                               "lightcoral", "SkyBlue2")
    # Labeling specs
    V(bigraph)$label.family <- "sans"
    V(bigraph)$label.font <- 2
    V(bigraph)$label.color <- "black"
    V(bigraph)$label.cex <- 2 * vertex_scale
    # Edge specs
    E(bigraph)$width <- .5 + 2.5 * vertex_scale
    # Return graph
    bigraph
  }

#' @rdname prettify
#' @export
prettify.an <- prettify_an
