#' @title Affiliation network structure
#'
#' @description Test `igraph` objects for affiliation network structure or
#'   impose such structure if possible.
#'
#' @details An affiliation network is a bipartite graph whose nodes are
#'   classified as actors and events in such a way that all links are between
#'   actors and events. The function [is_bipartite()] tests an `igraph` object
#'   for a `type` attribute, which is intended to bipartition of the nodes. It
#'   does not test whether the links respect this partition. The function
#'   `is_an` tests this, as well as the condition that actor nodes precede event
#'   nodes in their node IDs, which simplifies some other functions. The
#'   function `as_an` coerces an `igraph` object to an affiliation network by
#'   verifying that the object is bipartite and minimally permuting the node
#'   IDs. If `graph` has no `type` attribute and `map_type` is `FALSE`, then
#'   `as_an` throws an error; if `map_type` is `TRUE`, then `as_an` calls
#'   [bipartite_mapping()] to add a logical `type` attribute that takes the
#'   value `FALSE` at the first node (by node ID) in each connected component
#'   and `TRUE` or `FALSE` at the remaining nodes. (If this cannot be done, an
#'   error is thrown.)
#'
#' @name affiliation_network
#' @family network testing and coercion
#' @seealso Original **igraph** functions: [is_igraph()]
#' @param graph An `igraph` object.
#' @param map_type Logical; whether to add a `type` attribute for bipartite
#'   structure if `graph` admits one.
#' @returns For `is_an()`, a logical value; for `as_an()`, the input `graph`
#'   with a `"type"` attribute.
#' @examples
#' graph <- make_graph(c( 1,2, 1,4, 1,6, 3,6, 5,6, 7,8, 9,8 ))
#' is_an(graph)
#' # as_an(graph) # throws an error
#' an_graph <- as_an(graph, map_type = TRUE)
#' is_an(an_graph)
#' 
NULL

#' @rdname affiliation_network
#' @export
is_an <- function(graph) {
  if (! is_igraph(graph)) return(FALSE)
  if (! is_simple(graph)) return(FALSE)
  if (is_directed(graph)) return(FALSE)
  if (! ("type" %in% vertex_attr_names(graph))) return(FALSE)
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
as_an <- function(graph, map_type = FALSE) {
  if (! is_igraph(graph)) stop("Not an igraph object.")
  if (! is_simple(graph)) {
    graph <- simplify(graph)
  }
  if (is_directed(graph)) {
    warning("Graph is directed; collapsing directed links.")
    graph <- as_undirected(graph, mode = "collapse")
  }
  if (! ("type" %in% vertex_attr_names(graph))) {
    if (map_type) {
      an_map <- bipartite_mapping(graph)
      if (an_map$res) {
        vertex_attr(graph, "type") <- an_map$type
      } else {
        stop("`graph` does not admit a bipartition.")
      }
    } else {
      stop("`graph` needs a 'type' attribute.")
    }
  }
  el <- as_edgelist(graph, names = FALSE)
  if (! all(as.numeric(V(graph)$type[el[, 1]]) +
           as.numeric(V(graph)$type[el[, 2]]) == 1)) {
    stop("Attribute 'type' does not form a bipartition.")
  }
  permute(graph, order(order(V(graph)$type)))
}

#' @rdname affiliation_network
#' @export
as.an <- as_an
