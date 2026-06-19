#' @title Affiliation network structure
#'   
#' @description Test `igraph` objects for affiliation network structure or
#'   impose such structure if possible.
#'   
#' @details An affiliation network is a bipartite graph whose nodes are 
#'   classified as actors and events in such a way that all links are between 
#'   actors and events. The function [is_bipartite] tests an
#'   `igraph` object for a `type` attribute, which is intended to 
#'   bipartition of the nodes. It does not test whether the links respect this 
#'   partition. The function `is_an` tests this, as well as the condition 
#'   that actor nodes precede event nodes in their node IDs, which simplifies 
#'   some other functions. The function `as_an` coerces an `igraph` object 
#'   to an affiliation network by verifying that the object is bipartite and 
#'   minimally permuting the node IDs. If `graph` has no `type` 
#'   attribute and `add.type.attribute` is `FALSE`, then `as_an` 
#'   throws an error; if `add.type.attribute` is `TRUE`, then 
#'   `as_an` introduces a logical `type` attribute that takes the 
#'   value `FALSE` at the first node (by node ID) in each connected 
#'   component and `TRUE` or `FALSE` at the remaining nodes according 
#'   as they are an odd or even number of hops from the first.
#'   
#' @name affiliation_network
#' @family network testing and coercion
#' @seealso Original **igraph** functions: [is_igraph]
#' @param graph An `igraph` object.
#' @param add.type.attribute Logical; whether to introduce a `type` 
#'   attribute if `graph` has none before testing for bipartite structure.
NULL

#' @rdname affiliation_network
#' @export
is_an <- function(graph) {
  if (!is_igraph(graph)) return(FALSE)
  if (!is_simple(graph)) return(FALSE)
  if (is_directed(graph)) return(FALSE)
  if (!("type" %in% vertex_attr_names(graph))) return(FALSE)
  if (is.unsorted(V(graph)$type)) return(FALSE)
  el <- as_edgelist(graph, names = FALSE)
  all(as.numeric(V(graph)$type[el[, 1]]) +
        as.numeric(V(graph)$type[el[, 2]]) == 1)
}

#' @rdname affiliation_network
#' @export
is.an <- is_an

#' @rdname affiliation_network
#' @export
as_an <- function(graph, add.type.attribute = FALSE) {
  if (!is_igraph(graph)) stop("Not an igraph object.")
  if (!is_simple(graph)) {
    graph <- simplify(graph)
  }
  if (is_directed(graph)) {
    warning("Graph is directed; collapsing directed links.")
    graph <- as_undirected(graph, mode = "collapse")
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
as.an <- as_an

try_type <- function(graph) {
  comps <- components(graph)
  start_nodes <- which(!duplicated(comps$membership))
  dist_mat <- distances(graph, v = start_nodes, weights = NA)
  dist_mat[is.infinite(dist_mat)] <- NA
  dists <- apply(dist_mat, 2, sum, na.rm = TRUE)
  vertex_attr(graph) <- c(vertex_attr(graph), list("type" = logical(0)))
  graph
}
