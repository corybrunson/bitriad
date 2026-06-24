#' @title Dynamic affiliation network structure
#'
#' @description An affiliation network is *dynamic*, for present purposes, if
#'   its event nodes have time stamps, recorded as a numeric vertex attribute
#'   `"time"` in non-decreasing order.
#'
#' @name dynamic_an
#' @family network testing and coercion
#' @param graph An affiliation network.
#' @param use_attr Character; the vertex attribute to coerce to numeric if
#'   necessary and use as the `"time"` attribute. If `NULL`, the default, time
#'   takes the values `seq(event_count(graph))`, reflecting the order of the
#'   event node IDs.
#' @returns The input `graph` with a new `"time"` vertex attribute and its event
#'   nodes permuted to respect this attribute.
#' @examples
#' data(women_group)
#' is_dynamic_an(women_group)
#' data(women_clique)
#' is_dynamic_an(women_clique)
#' as_dynamic_an(women_clique)
#' data(nmt_meetings)
#' nmt_meetings <- set_event_attr(
#'   nmt_meetings, "meet",
#'   value = gsub("meet", "", V2(nmt_meetings)$name)
#' )
#' as_dynamic_an(nmt_meetings, use_attr = "meet")
#' 
NULL

#' @rdname dynamic_an
#' @export
is_dynamic_an <- function(graph) {
  if (! is_an(graph)) return(FALSE)
  if (vcount(graph) == 0) return(TRUE)
  if (! ("time" %in% vertex_attr_names(graph))) return(FALSE)
  if (! is.numeric(vertex_attr(graph, "time"))) return(FALSE)
  ! (is.unsorted(vertex_attr(graph, "time",
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
  graph, use_attr = NULL
) {
  if (! is_igraph(graph)) stop("Not an igraph object.")
  if (vcount(graph) == 0) return(graph)
  graph <- as_an(graph)
  
  if (is.null(use_attr)) {
    
    if (is_dynamic_an(graph)) {
      warning("`graph` is already dynamic and will be returned unchanged.")
    } else {
      graph <- set_event_attr(
        graph, "time",
        value = seq(event_count(graph))
      )
    }
    
  } else {
    
    if (! use_attr %in% vertex_attr_names(graph)) {
      message(
        "No vertex attribute '", use_attr,
        "'; inferring 'time' from event node indices."
      )
      graph <- set_event_attr(
        graph, "time",
        value = seq(event_count(graph))
      )
      
    } else {
      
      graph <- set_event_attr(
        graph, "time",
        value = as.numeric(event_attr(graph, use_attr))
      )
      graph <- permute(graph, c(
        seq(actor_count(graph)),
        actor_count(graph) +
          order(order(event_attr(graph, "time"))))
      )
      
    }
    
  }
  
  graph
}
