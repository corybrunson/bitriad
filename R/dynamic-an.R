#' @title Dynamic affiliation network structure
#'   
#' @description An affiliation network is \emph{dynamic}, for present purposes, 
#'   if its event nodes have time stamps, recorded as a numeric vertex attribute
#'   \code{"time"} in non-decreasing order.
#'   
#' @name dynamic_an
#' @family network testing and coercion
#' @param graph An affiliation network.
#' @param use.time.attribute Character; the vertex attribute to coerce to 
#'   numeric if necessary and use as the \code{"time"} attribute.
#' @param add.time.attribute Logical; whether, if \code{graph} has no attribute
#'   \code{use.time.attribute}, to introduce an artificial \code{"time"}
#'   attribute taking the values \code{1:event_count(graph)}, reflecting the
#'   order of the event node IDs.
NULL

#' @rdname dynamic_an
#' @export
is_dynamic_an <- function(graph) {
  if (!is_an(graph)) return(FALSE)
  if (vcount(graph) == 0) return(TRUE)
  if (!("time" %in% vertex_attr_names(graph))) return(FALSE)
  if (!is.numeric(vertex_attr(graph, "time"))) return(FALSE)
  !(is.unsorted(vertex_attr(graph, "time",
                            V(graph)[V(graph)$type == TRUE]),
                na.rm = TRUE))
}

#' @rdname dynamic_an
#' @export
is.dyn <- function(graph) {
  .Deprecated("is_dynamic_an")
  is_dynamic_an(graph)
}

#' @rdname dynamic_an
#' @export
as_dynamic_an <- function(
  graph, use.time.attribute = "time", add.time.attribute = FALSE
) {
  if (!is_igraph(graph)) stop("Not an igraph object.")
  if (vcount(graph) == 0) return(graph)
  graph <- as_an(graph)
  
  if (!use.time.attribute %in% vertex_attr_names(graph)) {
    if (add.time.attribute) {
      warning("No vertex attribute '", use.time.attribute, "'")
      set_vertex_attr(graph, "time",
                      c(rep(NA, actor_count(graph)), 1:event_count(graph)))
      use.time.attribute <- "time"
    } else {
      stop("No vertex attribute '", use.time.attribute, "'")
    }
  }
  
  if (use.time.attribute != "time") {
    if ("time" %in% vertex_attr_names(graph)) {
      stop("'graph' has a 'time' attribute.")
    }
    set_vertex_attr(graph, "time",
                    c(rep(NA, actor_count(graph)),
                      as.numeric(vertex_attr(graph, add.time.attribute,
                                             V(graph)[V(graph)$type == TRUE]))))
  }
  
  permute(graph, c(
    1:actor_count(graph),
    actor_count(graph) +
      order(order(vertex_attr(graph, "time")[V(graph)$type == TRUE])))
  )
}
