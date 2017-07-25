#' @title Triad closure for affiliation networks
#'   
#' @description Given an affiliation network and a vector of actor node IDs,
#'   calculate a specified flavor of triad closure centered at the nodes.
#'   
#' @details The \code{triadclosure_*} functions implement the several flavors of
#'   triad closure described below. Each function returns a single global 
#'   statistic, a vector of local statistics, or a matrix of local denominators 
#'   and numerators from which the global and local statistics can be recovered.
#'   
#' @template triadclosure
#'   

#' @name triadclosure_an
#' @family triad closure
#' @param bigraph An affiliation network.
#' @param actors A vector of actor nodes in \code{bigraph}.
#' @param type The type of statistic, matched to \code{"global"}, 
#'   \code{"local"}, or \code{"raw"}.
#' @param ... Flavor specifications passed to \code{\link{wedges_an}}.
#' @param wedges.fun A custom wedge census function. It must accept an 
#'   affiliation network \code{bigraph} and a single actor node ID \code{actor} 
#'   and may have any additional parameters. It must return a named list with 
#'   values \code{wedges} a numeric matrix of node IDs whose columns record the
#'   wedges centered at \code{actor} and \code{closed} a logical vector
#'   recording whether each wedge is closed.
#' @return If \code{type} is \code{"global"}, the global statistic for 
#'   \code{bigraph}; if \code{"local"}, the local statistics for \code{actors}; 
#'   if \code{"raw"}, a 2-column matrix, each row of which gives the number of 
#'   wedges and of closed wedges centered at \code{actors}.
#' @examples
#' data(women_clique)
#' mapply(
#'   triadclosure_an,
#'   wedges.fun = c("watts_strogatz", "opsahl", "exclusive"),
#'   MoreArgs = list(bigraph = women_clique, type = "local")
#' )
#' data(women_group)
#' cbind(
#'     triadclosure_watts_strogatz(women_group, type = "local"),
#'     triadclosure_opsahl(women_group, type = "local"),
#'     triadclosure_exclusive(women_group, type = "local")
#' )
#' @export
triadclosure_an <- function(
  bigraph, actors = V(bigraph)[V(bigraph)$type == FALSE],
  type = "global",
  ...,
  wedges.fun = NULL
) {
  type <- match.arg(type, c("global", "local", "raw"))
  if (type == "global" &&
      !setequal(V(bigraph)[V(bigraph)$type == FALSE], V(bigraph)[actors])) {
    warning("Calculating a global statistic on a subset of actors.")
  }
  wedges_fun <- if (is.null(wedges.fun)) {
    wedges_an
  } else if (is.character(wedges.fun)) {
    get(paste0("wedges_", wedges.fun))
  } else {
    wedges.fun
  }
  wedges <- sapply(actors, function(actor) {
    wc <- wedges_fun(bigraph, actor, ...)$closed
    c(length(wc), sum(wc))
  })
  wedgeReturn(wedges = t(wedges), type = type)
}

#' @rdname triadclosure_an
#' @export
triadclosure_watts_strogatz <- function(
  bigraph, actors = V(bigraph)[V(bigraph)$type == FALSE], type = "global"
) triadclosure_an(
  bigraph = bigraph, actors = actors, type = type,
  alcove = 0, wedge = 0, maps = 0, congruence = 2
)

#' @rdname triadclosure_an
#' @export
triadclosure_classical <- triadclosure_watts_strogatz

#' @rdname triadclosure_an
#' @export
triadclosure_opsahl <- function(
  bigraph, actors = V(bigraph)[V(bigraph)$type == FALSE], type = "global"
) triadclosure_an(
  bigraph = bigraph, actors = actors, type = type,
  alcove = 0, wedge = 0, maps = 1, congruence = 0
)

#' @rdname triadclosure_an
#' @export
triadclosure_twomode <- triadclosure_opsahl

#' @rdname triadclosure_an
#' @export
triadclosure_liebig_rao_0 <- function(
  bigraph, actors = V(bigraph)[V(bigraph)$type == FALSE], type = "global"
) triadclosure_an(
  bigraph = bigraph, actors = actors, type = type,
  alcove = 0, wedge = 0, maps = 2, congruence = 0
)

#' @rdname triadclosure_an
#' @export
triadclosure_unconnected <- triadclosure_liebig_rao_0

#' @rdname triadclosure_an
#' @export
triadclosure_liebig_rao_3 <- function(
  bigraph, actors = V(bigraph)[V(bigraph)$type == FALSE], type = "global"
) triadclosure_an(
  bigraph = bigraph, actors = actors, type = type,
  alcove = 3, wedge = 2, maps = 2, congruence = 0
)

#' @rdname triadclosure_an
#' @export
triadclosure_completely_connected <- triadclosure_liebig_rao_3

#' @rdname triadclosure_an
#' @export
triadclosure_exclusive <- function(
  bigraph, actors = V(bigraph)[V(bigraph)$type == FALSE], type = "global"
) triadclosure_an(
  bigraph = bigraph, actors = actors, type = type,
  alcove = 0, wedge = 0, maps = 2, congruence = 1
)
