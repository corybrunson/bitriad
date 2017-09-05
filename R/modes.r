#' @title Actor and event node iteration and attribute querying and assignment
#'   
#' @description These functions return actor and event node lists.
#'   
#' @name modes
#' @param graph An affiliation network.
#' @param name  The name of the attribute to set.
#' @param index An optional node sequence to set the attributes of a subset of 
#'   actor or event nodes.
#' @param x An affiliation network.
#' @param value The new value of the attribute for all (or \code{index}) actor 
#'   or event nodes.
#' @return \code{graph}, with the actor or event attribute added or set.
#' @examples
#' data(women_clique)
#' V1(women_clique)
#' V2(women_clique)
#' @seealso \code{\link[igraph]{V}}, \code{\link[igraph]{set_vertex_attr}}
NULL

#' @rdname modes
#' @export
V1 <- function(graph) {
  stopifnot(is_an(graph))
  V(graph)[V(graph)$type == FALSE]
}

#' @rdname modes
#' @export
V2 <- function(graph) {
  stopifnot(is_an(graph))
  V(graph)[V(graph)$type == TRUE]
}

#' @rdname modes
#' @export
set_actor_attr <- function(graph, name, index = V1(graph), value) {
  stopifnot(is_an(graph))
  set_vertex_attr(graph = graph, name = name, index = index, value = value)
}

#' @rdname modes
#' @export
set_event_attr <- function(graph, name, index = V2(graph), value) {
  stopifnot(is_an(graph))
  set_vertex_attr(graph = graph, name = name, index = index, value = value)
}

#' x = an affiliation network
#' value = V1(x) or V2(x) with additional attributes:
#'   "name" = name of attribute being assigned
#'   "value" = values being assigned to attribute

#' @rdname modes
#' @export
`V1<-` <- function(x, value) {
  stopifnot(is_an(x))
  if (! "name"  %in% names(attributes(value)) ||
      ! "value" %in% names(attributes(value))) {
    stop("invalid indexing")
  }
  if (attr(value, "name") == "type") {
    stop("Node attribute 'type' is intrinsic to affiliation network structure.")
  }
  set_actor_attr(x, attr(value, "name"), index = value,
                 value = attr(value, "value"))
}

#' @rdname modes
#' @export
`V2<-` <- function(x, value) {
  stopifnot(is_an(x))
  if (! "name"  %in% names(attributes(value)) ||
      ! "value" %in% names(attributes(value))) {
    stop("invalid indexing")
  }
  if (attr(value, "name") == "type") {
    stop("Node attribute 'type' is intrinsic to affiliation network structure.")
  }
  set_event_attr(x, attr(value, "name"), index = value,
                 value = attr(value, "value"))
}
