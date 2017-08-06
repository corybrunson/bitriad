#' @title Convenient plotting aesthetics for affiliation networks
#'   
#' @description Given an affiliation network, assign the node and link
#'   aesthetics to values that produce a neater visualization through
#'   \code{\link[igraph]{plot.igraph}} than the \strong{igraph} defaults.
#'   
#' @name prettify
#' @param bigraph An affiliation network.
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
    V(bigraph)$shape <- ifelse(V(bigraph)$type, "square", "circle")
    V(bigraph)$size <- ifelse(V(bigraph)$type, 28, 32) * vertex_scale
    V(bigraph)$color <- ifelse(V(bigraph)$type, "lightcoral", "SkyBlue2")
    # Labeling specs
    V(bigraph)$label.family <- "sans"
    V(bigraph)$label.font <- 2
    V(bigraph)$label.color <- "black"
    V(bigraph)$label.cex <- 2 * vertex_scale
    # Edge specs
    E(bigraph)$width <- .5 + 2.5 * vertex_scale
    E(bigraph)$color <- "darkgrey"
    # Return graph
    bigraph
  }

#' @rdname prettify
#' @export
prettify.an <- prettify_an
