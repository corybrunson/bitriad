#' @title Triad closure in affiliation networks
#'   
#' @description Given an affiliation network and a vector of node IDs, calculate
#'   the (proportion of) triad closure of a specified flavor at each node. See 
#'   \code{\link{wedges_an}} and \code{triads_an} for details.
#'   
#' @name triadclosure_an
#' @family triad closure
#' @param bigraph An affiliation network.
#' @param actors Actor nodes in \code{bigraph}; (re)set to \code{V(bigraph)} if 
#'   \code{type} is \code{"global"}.
#' @param type The type of statistic, matched to \code{"global"}, 
#'   \code{"local"}, or \code{"raw"}.
#' @param ... Flavor specifications passed to \code{\link{wedges_an}}.
#' @return A numeric vector of length \code{length(actors)} giving the value of 
#'   the specified local triad closure statistic at each actor node.
#' @return If \code{type} is \code{"global"}, the global statistic for 
#'   \code{bigraph}; if \code{"local"}, the local statistics for \code{actors}; 
#'   if \code{"raw"}, a 2-column matrix, each row of which gives the number of
#'   wedges and of closed wedges centered at \code{actors}.
#' @export
triadclosure_an <- function(
  bigraph, actors = V(bigraph)[V(bigraph)$type == FALSE],
  type = "global",
  ...
) {
  type <- match.arg(type, c("global", "local", "raw"))
  if (type == "global" &&
      !setequal(V(bigraph)[V(bigraph)$type == FALSE], V(bigraph)[actors])) {
    warning("Calculating a global statistic on a subset of actors.")
  }
  wedges <- sapply(actors, function(actor) {
    wc <- wedges_an(bigraph, actor, ...)$closed
    c(length(wc), sum(wc))
  })
  wedgeReturn(wedges = t(wedges), type = type)
}
