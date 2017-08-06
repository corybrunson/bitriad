#' @title Affiliation networks
#'   
#' @description Test \code{igraph} objects for affiliation network structure or
#'   impose such structure if possible.
#'   
#' @details An affiliation network is a bipartite graph whose nodes are 
#'   classified as actors and events in such a way that all links are between 
#'   actors and events. The function \code{\link[igraph]{is_bipartite}} tests an
#'   `igraph` object for a \code{type} attribute, which is intended to 
#'   bipartition of the nodes. It does not test whether the links respect this 
#'   partition. The function \code{is_an} tests this, as well as the condition 
#'   that actor nodes precede event nodes in their node IDs, which simplifies 
#'   some other functions. The function \code{as_an} coerces an `igraph` object 
#'   to an affiliation network by verifying that the object is bipartite and 
#'   minimally permuting the node IDs. If \code{igraph} has no \code{type} 
#'   attribute and \code{add.type.attribute} is \code{FALSE}, then \code{as_an} 
#'   throws an error; if \code{add.type.attribute} is \code{TRUE}, then 
#'   \code{as_an} introduces a logical \code{type} attribute that takes the 
#'   value \code{FALSE} at the first node (by node ID) in each connected 
#'   component and \code{TRUE} or \code{FALSE} at the remaining nodes according 
#'   as they are an odd or even number of hops from the first.
#'   
#' @name affiliation_network
#' @param graph An \code{igraph} object.
#' @param add.type.attribute Logical; whether to introduce a \code{type} 
#'   attribute if \code{igraph} has none before testing for bipartite structure.
#' @export
is_an <- function(graph) {
  if (!is_igraph(graph)) return(FALSE)
  if (vcount(graph) == 0) return(TRUE)
  if (!is_simple(graph)) return(FALSE)
  if (is_directed(graph)) {
    warning("Graph is directed.")
  }
  if (!("type" %in% vertex_attr_names(graph))) return(FALSE)
  if (is.unsorted(V(graph)$type)) return(FALSE)
  el <- as_edgelist(graph, names = FALSE)
  all(as.numeric(V(graph)$type[el[, 1]]) +
        as.numeric(V(graph)$type[el[, 2]]) == 1)
}

#' @rdname affiliation_network
#' @export
as_an <- function(graph, add.type.attribute = FALSE) {
  if (!is_igraph(graph)) stop("Not an igraph object.")
  if (vcount(graph) == 0) return(graph)
  if (!is_simple(graph)) {
    graph <- simplify(graph)
  }
  if (is_directed(graph)) {
    warning("Graph is directed; collapsing directed links.")
    graph <- as.undirected(graph, mode = "collapse")
  }
  if (!("type" %in% vertex_attr_names(graph))) {
    if (add.type.attribute) {
      graph <- try_type(graph)
    } else {
      stop("Needs 'type' attribute.")
    }
  }
  el <- as_edgelist(graph, names = FALSE)
  if (!all(as.numeric(V(graph)$type[el[, 1]]) +
           as.numeric(V(graph)$type[el[, 2]]) == 1)) {
    stop("Attribute 'type' does not form a bipartition.")
  }
  permute(graph, order(order(V(graph)$type)))
}

#' @rdname affiliation_network
#' @export
is.an <- is_an

#' @rdname affiliation_network
#' @export
as.an <- as_an

try_type <- function(graph) {
  comps <- components(graph)
  start_nodes <- which(!duplicated(comps$membership))
  dist_mat <- distances(graph, v = start_nodes, weights = NA)
  dist_mat[is.infinite(dist_mat)] <- NA
  dists <- apply(dist_mat, 2, sum, na.rm = TRUE)
  V(graph)$type <- as.logical(dists %% 2)
  graph
}
