#' @title Convenient plotting aesthetics for affiliation networks
#'   
#' @description Given an affiliation network, assign the node and link
#'   aesthetics to values that produce a neater visualization through
#'   \code{\link[igraph]{plot.igraph}} than the \strong{igraph} defaults.
#'   
#' @name prettify
#' @param graph An affiliation network.
#' @export
#' @examples
#' library(igraph)
#' data(women_clique)
#' data(whigs)
#' for (g in list(women_clique, whigs)) {
#'     plot(prettify_an(g))
#' }
prettify_an <-
  function(graph) {
    stopifnot(is_an(graph))
    # Use number of vertices to scale down vertex size
    vertex_scale <- max(1 / log(vcount(graph)), .001)
    # Vertex style specs
    V(graph)$shape <- ifelse(V(graph)$type, "square", "circle")
    V(graph)$size <- ifelse(V(graph)$type, 28, 32) * vertex_scale
    V(graph)$color <- ifelse(V(graph)$type, "lightcoral", "SkyBlue2")
    # Labeling specs
    V(graph)$label.family <- "sans"
    V(graph)$label.font <- 2
    V(graph)$label.color <- "black"
    V(graph)$label.cex <- 2 * vertex_scale
    # Edge specs
    E(graph)$width <- .5 + 2.5 * vertex_scale
    E(graph)$color <- "darkgrey"
    # Return graph
    graph
  }

#' @rdname prettify
#' @export
prettify.an <- prettify_an
