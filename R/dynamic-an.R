#' @title Dynamic affiliation networks
#'   
#' @description An affiliation network is \emph{dynamic}, for present purposes, 
#'   if its event nodes have time stamps, recorded as a numeric vertex attribute
#'   \code{"time"} in non-decreasing order.
#'   
#' @name dynamic_an
#' @param bigraph An affiliation network.
#' @param add.time.attribute Character or logical; if character, the existing 
#'   attribute to coerce to numeric if necessary and use as the time attribute;
#'   if logical, whether to introduce an artificial \code{time} attribute
#'   \code{1:event_count(bigraph)}, reflecting the order of the event node IDs,
#'   if \code{bigraph} has none.
#' @export
is_dynamic_an <- function(bigraph) {
  if (!is_an(bigraph)) return(FALSE)
  if (!("time" %in% vertex_attr_names(bigraph))) return(FALSE)
  if (!is.numeric(vertex_attr(bigraph, "time"))) return(FALSE)
  !(is.unsorted(vertex_attr(bigraph, "time",
                            V(bigraph)[V(bigraph)$type == TRUE])))
}

#' @rdname dynamic_an
#' @export
is.dyn <- function(bigraph) {
  .Deprecated("is_dynamic_an")
  is_dynamic_an(bigraph)
}

#' @rdname dynamic_an
#' @export
as_dynamic_an <- function(bigraph, add.time.attribute = FALSE) {
  if (!is_an(bigraph)) stop("Not an affiliation network.")
  if (!("time" %in% vertex_attr_names(bigraph))) {
    if (is.character(add.time.attribute)) {
      V(bigraph)$time <- c(
        rep(NA, actor_count(bigraph)),
        as.numeric(vertex_attr(bigraph, add.time.attribute,
                               V(bigraph)[V(bigraph)$type == TRUE]))
      )
    } else if (add.time.attribute) {
      V(bigraph)$time <- c(rep(NA, actor_count(bigraph)),
                           1:event_count(bigraph))
    } else {
      stop("Needs 'time' attribute.")
    }
  } else {
    if (is.character(add.time.attribute)) {
      warning("'bigraph' already has a 'time' attribute.")
    }
  }
  permute(bigraph, c(
    1:actor_count(bigraph),
    actor_count(bigraph) +
      order(order(V(bigraph)[V(bigraph)$type == TRUE]$time)))
  )
}
