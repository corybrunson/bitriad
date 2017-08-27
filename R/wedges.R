#' @title Wedge censuses and closure indicators for affiliation networks
#'   
#' @description Given an affiliation network and an actor node ID, identify all 
#'   wedges for a specified measure centered at the node and indicate whether 
#'   each is closed.
#'   
#' @details The \code{wedges_*} functions implement wedge censuses underlying 
#'   the several measures of triad closure described below. Each function 
#'   returns a transversal of wedges from the congruence classes of wedges 
#'   centered at the index actor and indicators of whether each class is closed.
#'   The shell function \code{wedges} determines a unique measure from several 
#'   coded arguments (see below) and passes the input affiliation network to 
#'   that measure.
#'   
#' @template triadclosure
#'   

#' @name wedges
#' @family wedge functions
#' @param graph An affiliation network.
#' @param actor An actor node in \code{graph}.
#' @param representation Character; the representation of the graph used in
#'   performing the wedge census. Matched to \code{"edgelist"} or
#'   \code{"adjlist"}; see \code{\link[igraph]{as_edgelist}} and
#'   \code{\link[igraph]{as_adj_list}}.
#' @param alcove,wedge,maps,congruence Choice of alcove, wedge, maps, and 
#'   congruence (see Details).
#' @return A two-element list consisting of (1) a 3- or 5-row integer matrix of 
#'   (representatives of) all (congruence classes of) wedges in \code{graph} 
#'   centered at \code{actor}, and (2) a logical vector indicating whether each 
#'   wedge is closed.
#' @export
wedges <- function(
  graph, actor,
  representation = "edgelist",
  alcove = 0, wedge = 0, maps = 0, congruence = 0
) {
  stopifnot(V(graph)[actor]$type == FALSE)
  representation <- match.arg(representation, c("adjlist", "edgelist"))
  repr <- if (representation == "adjlist") {
    unname(lapply(as_adj_list(graph), as.numeric))
  } else if (representation == "edgelist") {
    as_edgelist(graph, names = FALSE)
  }
  suffix <- paste0(
    "x", alcove,
    "w", wedge,
    "m", maps,
    "c", congruence
  )
  wedges_fun <- get(paste0("wedges_", representation, "_", suffix))
  wedges_fun(repr, as.numeric(V(graph)[actor]))
}

#' @rdname wedges
#' @export
wedges_an <- wedges

#' @rdname wedges
#' @export
wedges_watts_strogatz <- function(
  graph, actor,
  representation = "edgelist"
) wedges(
  graph = graph, actor = actor,
  representation = "edgelist",
  alcove = 0, wedge = 0, maps = 0, congruence = 2
)

#' @rdname wedges
#' @export
wedges_classical <- wedges_watts_strogatz

#' @rdname wedges
#' @export
wedges_projection <- wedges_watts_strogatz

#' @rdname wedges
#' @export
wedges_opsahl <- function(
  graph, actor,
  representation = "edgelist"
) wedges(
  graph = graph, actor = actor,
  representation = "edgelist",
  alcove = 0, wedge = 0, maps = 1, congruence = 0
)

#' @rdname wedges
#' @export
wedges_twomode <- wedges_opsahl

#' @rdname wedges
#' @export
wedges_liebig_rao_0 <- function(
  graph, actor,
  representation = "edgelist"
) wedges(
  graph = graph, actor = actor,
  representation = "edgelist",
  alcove = 0, wedge = 0, maps = 2, congruence = 0
)

#' @rdname wedges
#' @export
wedges_unconnected <- wedges_liebig_rao_0

#' @rdname wedges
#' @export
wedges_liebig_rao_3 <- function(
  graph, actor,
  representation = "edgelist"
) wedges(
  graph = graph, actor = actor,
  representation = "edgelist",
  alcove = 3, wedge = 2, maps = 2, congruence = 0
)

#' @rdname wedges
#' @export
wedges_completely_connected <- wedges_liebig_rao_3

#' @rdname wedges
#' @export
wedges_exclusive <- function(
  graph, actor,
  representation = "edgelist"
) wedges(
  graph = graph, actor = actor,
  representation = "edgelist",
  alcove = 0, wedge = 0, maps = 2, congruence = 1
)

wedges_edgelist_x0w0m2c2 <- wedges_edgelist_x0w0m2c1
