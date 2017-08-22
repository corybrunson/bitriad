#' @title Wedge censuses and closure indicators for dynamic affiliation networks
#'   
#' @description Given a dynamic affiliation network and an actor node ID, 
#'   identify all wedges for a specified measure centered at the node and 
#'   indicate whether each is closed.
#'   
#' @details The \code{dynamic_wedges_*} functions implement wedge censuses 
#'   underlying the several measures of triad closure described below. Each 
#'   function returns a transversal of wedges from the congruence classes of 
#'   wedges centered at the index actor and indicators of whether each class is 
#'   closed. The shell function \code{dynamic_wedges} determines a unique
#'   measure from several coded arguments (see below) and passes the input
#'   affiliation network to that measure.
#'   
#' @template triadclosure
#'   

#' @name dynamic_wedges
#' @family wedge functions
#' @param graph A dynamic affiliation network.
#' @param actor An actor node in \code{graph}.
#' @param alcove,wedge,maps,congruence Choice of alcove, wedge, maps, and 
#'   congruence (see Details).
#' @param memory Numeric; minimum delay of wedge formation since would-have-been
#'   closing events.
#' @param wedge.gap Numeric; maximum delay between the two events of a wedge.
#' @param close.after,close.before Numeric; minimum and maximum delays after
#'   both events form a wedge for a third event to close it.
#' @return A two-element list consisting of (1) a 3- or 5-row integer matrix of 
#'   (representatives of) all (congruence classes of) wedges in \code{graph} 
#'   centered at \code{actor}, and (2) a logical vector indicating whether each 
#'   wedge is closed.
#' @export
dynamic_wedges <- function(
  graph, actor,
  alcove = 0, wedge = 0, maps = 0, congruence = 0,
  memory = Inf, wedge.gap = Inf, close.after = 0, close.before = Inf
) {
  warning("'dynamic_wedges' is experimental.")
  stopifnot(V(graph)[actor]$type == FALSE)
  suffix <- paste0(c(
    "x", alcove,
    "w", wedge,
    "m", maps,
    "c", congruence
  ), collapse = "")
  wedges_fun <- get(paste0("dynamic_wedges_", suffix))
  wedges_fun(
    el = as_edgelist(graph, names = FALSE),
    t = V(graph)$time,
    q = as.numeric(V(graph)[actor]),
    memory = memory,
    wedge_gap = wedge.gap,
    close_after = close.after, close_before = close.before
  )
}
