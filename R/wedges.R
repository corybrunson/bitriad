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
#' @param alcove,wedge,maps,congruence Choice of alcove, wedge, maps, and 
#'   congruence (see Details).
#' @param w,x,y,z Integer vectors of the same length, indicating the number of
#'   events of each structural equivalence class in a triad of three actors
#'   \code{p}, \code{q}, \code{r}: \code{w} attended by all three, \code{x}
#'   attended by \code{p} and \code{q} only, \code{y} attended by \code{q} and
#'   \code{r} only, and \code{z} attended by \code{p} and \code{r} only.
#' @return A two-element list consisting of (1) a 3- or 5-row integer matrix of 
#'   (representatives of) all (congruence classes of) wedges in \code{graph} 
#'   centered at \code{actor}, and (2) a logical vector indicating whether each 
#'   wedge is closed.
#' @export
wedges <- function(
  graph, actor,
  alcove = 0, wedge = 0, maps = 0, congruence = 0
) {
  stopifnot(V(graph)[actor]$type == FALSE)
  suffix <- paste0(
    "x", alcove,
    "w", wedge,
    "m", maps,
    "c", congruence
  )
  wedges_fun <- get(paste0("wedges_", suffix))
  wedges_fun(
    el = as_edgelist(graph, names = FALSE),
    q = as.numeric(V(graph)[actor])
  )
}

#' @rdname wedges
#' @export
wedges_an <- wedges

wedges_x0w0m2c2 <- wedges_x0w0m2c1

#' @rdname wedges
#' @export
wedges_watts_strogatz <- function(graph, actor) wedges(
  graph = graph, actor = actor,
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
wedges_opsahl <- function(graph, actor) wedges(
  graph = graph, actor = actor,
  alcove = 0, wedge = 0, maps = 1, congruence = 0
)

#' @rdname wedges
#' @export
wedges_twomode <- wedges_opsahl

#' @rdname wedges
#' @export
wedges_liebig_rao_0 <- function(graph, actor) wedges(
  graph = graph, actor = actor,
  alcove = 0, wedge = 0, maps = 2, congruence = 0
)

#' @rdname wedges
#' @export
wedges_unconnected <- wedges_liebig_rao_0

#' @rdname wedges
#' @export
wedges_liebig_rao_3 <- function(graph, actor) wedges(
  graph = graph, actor = actor,
  alcove = 3, wedge = 2, maps = 2, congruence = 0
)

#' @rdname wedges
#' @export
wedges_completely_connected <- wedges_liebig_rao_3

#' @rdname wedges
#' @export
wedges_exclusive <- function(graph, actor) wedges(
  graph = graph, actor = actor,
  alcove = 0, wedge = 0, maps = 2, congruence = 1
)

#' @rdname wedges
#' @export
centered_triads <- function(graph, actor) {
  stopifnot(V(graph)[actor]$type == FALSE)
  ct <- centered_triads_C(
    el = as_edgelist(graph, names = FALSE),
    q = as.numeric(V(graph)[actor])
  )
  rownames(ct) <- c("w", "x", "y", "z")
  ct
}

#' @rdname wedges
#' @export
triad_wedges_watts_strogatz <- function(w, x, y, z) cbind(
  wedges = (w > 0) | (x > 0 & y > 0),
  closed = (w > 0) | (x > 0 & y > 0 & z > 0)
)

#' @rdname wedges
#' @export
triad_wedges_classical <- triad_wedges_watts_strogatz

#' @rdname wedges
#' @export
triad_wedges_projection <- triad_wedges_watts_strogatz

#' @rdname wedges
#' @export
triad_wedges_opsahl <- function(w, x, y, z) cbind(
  wedges = x * y + (x + y) * w + w * (w - 1),
  closed = x * y * (w + z > 0) +
    (x + y) * w * (w - 1 + z > 0) +
    w * (w - 1) * (w - 2 + z > 0)
)

#' @rdname wedges
#' @export
triad_wedges_twomode <- triad_wedges_opsahl

#' @rdname wedges
#' @export
triad_wedges_liebig_rao_0 <- function(w, x, y, z) cbind(
  wedges = x * y,
  closed = x * y * (z > 0)
)

#' @rdname wedges
#' @export
triad_wedges_unconnected <- triad_wedges_liebig_rao_0

#' @rdname wedges
#' @export
triad_wedges_liebig_rao_1 <- function(w, x, y, z) cbind(
  wedges = x * y + (x + y) * w,
  closed = x * y * (w > 0) + (x + y) * w * (z > 0)
)

#' @rdname wedges
#' @export
triad_wedges_sparsely_connected <- triad_wedges_liebig_rao_1

#' @rdname wedges
#' @export
triad_wedges_liebig_rao_2 <- function(w, x, y, z) cbind(
  wedges = (x + y) * w + w * (w - 1),
  closed = (x + y) * w * (w > 1) + w * (w - 1) * (z > 0)
)

#' @rdname wedges
#' @export
triad_wedges_highly_connected <- triad_wedges_liebig_rao_2

#' @rdname wedges
#' @export
triad_wedges_liebig_rao_3 <- function(w, x, y, z) cbind(
  wedges = w * (w - 1),
  closed = w * (w - 1) * (w > 2)
)

#' @rdname wedges
#' @export
triad_wedges_completely_connected <- triad_wedges_liebig_rao_3

#' @rdname wedges
#' @export
triad_wedges_exclusive <- function(w, x, y, z) cbind(
  wedges = (x > 0) * (y > 0),
  closed = (x > 0) * (y > 0) * (z > 0)
)

#' @rdname wedges
#' @export
triad_wedges <- function(
  w, x, y, z,
  alcove = 0, wedge = 0, maps = 0, congruence = 0
) {
  wedgecount <- list(
    inin = (if (congruence == 0) w * (w - 1) else 1) *
      (wedge == 2 | maps != 2),
    inex = (if (congruence == 0) w * y else 1) *
      (wedge == 1 | (wedge == 0 & maps != 2)),
    exin = (if (congruence == 0) x * w else 1) *
      (wedge == 1 | (wedge == 0 & maps != 2)),
    exex = (if (congruence == 0) x * y else 1) *
      (wedge == 0)
  )
  wedges <- Reduce(if (congruence == 0) `+` else any, wedgecount)
  stop("Not yet implemented.")
  #cbind(wedges, closed)
}
