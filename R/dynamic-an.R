#' @title Dynamic affiliation networks
#'   
#' @description An affiliation network is \emph{dynamic}, for present purposes, 
#'   if its event nodes have time stamps, recorded as a numeric vertex attribute
#'   \code{"time"} in non-decreasing order.
#'   
#' @name dynamic_an
#' @param graph An affiliation network.
#' @param add.time.attribute Character or logical; if character, the existing 
#'   attribute to coerce to numeric if necessary and use as the time attribute;
#'   if logical, whether to introduce an artificial \code{time} attribute
#'   \code{1:event_count(graph)}, reflecting the order of the event node IDs,
#'   if \code{graph} has none.
#' @export
is_dynamic_an <- function(graph) {
  if (!is_an(graph)) return(FALSE)
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
as_dynamic_an <- function(graph, add.time.attribute = FALSE) {
  if (!is_an(graph)) stop("Not an affiliation network.")
  if (!("time" %in% vertex_attr_names(graph))) {
    if (is.character(add.time.attribute)) {
      V(graph)$time <- c(
        rep(NA, actor_count(graph)),
        as.numeric(vertex_attr(graph, add.time.attribute,
                               V(graph)[V(graph)$type == TRUE]))
      )
    } else if (add.time.attribute) {
      V(graph)$time <- c(rep(NA, actor_count(graph)),
                           1:event_count(graph))
    } else {
      stop("Needs 'time' attribute.")
    }
  } else {
    if (is.character(add.time.attribute)) {
      warning("'graph' already has a 'time' attribute.")
    }
  }
  permute(graph, c(
    1:actor_count(graph),
    actor_count(graph) +
      order(order(V(graph)[V(graph)$type == TRUE]$time)))
  )
}
