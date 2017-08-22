#' @title Triad closure for affiliation networks
#'   
#' @description Given an affiliation network and a vector of actor node IDs, 
#'   calculate a specified measure of triad closure centered at the nodes.
#'   
#' @details The \code{triad_closure_*} functions implement the several measures 
#'   of triad closure described below. Each function returns a single global 
#'   statistic, a vector of local statistics, or a matrix of local denominators 
#'   and numerators from which the global and local statistics can be recovered.
#'   
#'   The function \code{triad_closure_projection} recapitulates 
#'   \code{\link{triad_closure_watts_strogatz}} by invoking the 
#'   \code{\link[igraph]{bipartite_projection}} and 
#'   \code{\link[igraph]{transitivity}} functions in \strong{igraph}.
#'   
#' @template triadclosure
#'   

#' @name triad_closure
#' @family triad closure
#' @param graph An affiliation network.
#' @param actors A vector of actor nodes in \code{graph}.
#' @param type The type of statistic, matched to \code{"global"}, 
#'   \code{"local"}, or \code{"raw"}.
#' @param ... Measure specifications passed to \code{\link{wedges}}.
#' @param measure Character; the measure of triad closure, used as the suffix 
#'   \code{*} to \code{triad_closure_*} Matched to \code{"classical"} (also 
#'   \code{"watts_strogatz"}), \code{"twomode"} (also \code{"opsahl"}), 
#'   \code{"unconnected"} (also \code{"liebig_rao_0"}), 
#'   \code{"completely_connected"} (also \code{"liebig_rao_3"}), or 
#'   \code{"exclusive"}.
#' @param wedges.fun A custom wedge census function. It must accept an 
#'   affiliation network \code{graph} and a single actor node ID \code{actor} 
#'   and may have any additional parameters. It must return a named list with 
#'   values \code{wedges} a numeric matrix of node IDs whose columns record the 
#'   wedges centered at \code{actor} and \code{closed} a logical vector 
#'   recording whether each wedge is closed. Overrides \code{measure}.
#' @return If \code{type} is \code{"global"}, the global statistic for 
#'   \code{graph}; if \code{"local"}, the local statistics for \code{actors}; 
#'   if \code{"raw"}, a 2-column matrix, each row of which gives the number of 
#'   wedges and of closed wedges centered at \code{actors}.
#' @examples
#' data(women_clique)
#' mapply(
#'   triad_closure,
#'   measure = c("classical", "twomode", "unconnected", "exclusive"),
#'   MoreArgs = list(graph = women_clique, type = "local")
#' )
#' data(women_group)
#' cbind(
#'   triad_closure_watts_strogatz(women_group, type = "local"),
#'   triad_closure_opsahl(women_group, type = "local"),
#'   triad_closure_liebig_rao_0(women_group, type = "local"),
#'   triad_closure_exclusive(women_group, type = "local")
#' )
#' @export
triad_closure <- function(
  graph, actors = V(graph)[V(graph)$type == FALSE],
  type = "global",
  ...,
  measure = NULL,
  wedges.fun = NULL
) {
  if (!is_an(graph)) {
    stop("Not an affiliation network.")
  }
  type <- match.arg(type, c("global", "local", "raw"))
  if (type == "global" &&
      !setequal(V(graph)[V(graph)$type == FALSE], V(graph)[actors])) {
    warning("Calculating a global statistic on a subset of actors.")
  }
  wedges_fun <- if (!is.null(wedges.fun)) {
    wedges.fun
  } else if (!is.null(measure)) {
    measure <- match.arg(measure, c("classical", "watts_strogatz",
                                    "twomode", "opsahl",
                                    "unconnected", "liebig_rao_0",
                                    "completely_connected", "liebig_rao_3",
                                    "exclusive"))
    get(paste0("wedges_", measure))
  } else {
    wedges
  }
  wedges <- sapply(actors, function(actor) {
    wc <- wedges_fun(graph, actor, ...)$closed
    c(length(wc), sum(wc))
  })
  wedgeReturn(wedges = t(wedges), type = type)
}

#' @rdname triad_closure
#' @export
triad_closure_an <- triad_closure

#' @rdname triad_closure
#' @export
triad_closure_watts_strogatz <- function(
  graph, actors = V(graph)[V(graph)$type == FALSE], type = "global"
) triad_closure(
  graph = graph, actors = actors, type = type,
  alcove = 0, wedge = 0, maps = 0, congruence = 2
)

#' @rdname triad_closure
#' @export
triad_closure_classical <- triad_closure_watts_strogatz

#' @rdname triad_closure
#' @export
triad_closure_opsahl <- function(
  graph, actors = V(graph)[V(graph)$type == FALSE], type = "global"
) triad_closure(
  graph = graph, actors = actors, type = type,
  alcove = 0, wedge = 0, maps = 1, congruence = 0
)

#' @rdname triad_closure
#' @export
triad_closure_twomode <- triad_closure_opsahl

#' @rdname triad_closure
#' @export
triad_closure_liebig_rao_0 <- function(
  graph, actors = V(graph)[V(graph)$type == FALSE], type = "global"
) triad_closure(
  graph = graph, actors = actors, type = type,
  alcove = 0, wedge = 0, maps = 2, congruence = 0
)

#' @rdname triad_closure
#' @export
triad_closure_unconnected <- triad_closure_liebig_rao_0

#' @rdname triad_closure
#' @export
triad_closure_liebig_rao_3 <- function(
  graph, actors = V(graph)[V(graph)$type == FALSE], type = "global"
) triad_closure(
  graph = graph, actors = actors, type = type,
  alcove = 3, wedge = 2, maps = 2, congruence = 0
)

#' @rdname triad_closure
#' @export
triad_closure_completely_connected <- triad_closure_liebig_rao_3

#' @rdname triad_closure
#' @export
triad_closure_exclusive <- function(
  graph, actors = V(graph)[V(graph)$type == FALSE], type = "global"
) triad_closure(
  graph = graph, actors = actors, type = type,
  alcove = 0, wedge = 0, maps = 2, congruence = 1
)

# compress a wedgelist into a desired statistic
wedgeReturn <- function(wedges, type, add.names) {
  
  # global
  if (type == "global") {
    return(sum(wedges[, 2]) / sum(wedges[, 1]))
  }
  # local
  if (type == "local") {
    return(as.vector(wedges[, 2] / wedges[, 1]))
  }
  # otherwise
  #if (add.names) {
  #  rownames(wedges) <- V(graph)$name[vids]
  #  colnames(wedges) <- c("Wedges", "Closed")
  #}
  wedges
}

#' @rdname triad_closure
#' @export
triad_closure_projection <- function(
  graph, actors = V(graph)[V(graph)$type == FALSE],
  type = "global"
) {
  type <- match.arg(type, c("global", "local", "raw"))
  if (vcount(graph) == 0) {
    if (type == "global") {
      return(NaN)
    } else if (type == "local") {
      return(NULL)
    } else {
      return(matrix(NA, nrow = 0, ncol = 2))
    }
  }
  stopifnot(all(V(graph)$type[actors] == FALSE))
  proj <- actor_projection(graph)
  proj_actors <- which(which(!V(graph)$type) %in% actors)
  stopifnot(length(proj_actors) == length(actors))
  if (type == "global") {
    return(transitivity(proj, type = "global"))
  } else if (type == "local") {
    return(transitivity(proj, type = "local", vids = proj_actors))
  } else {
    C <- transitivity(proj, type = "local", vids = proj_actors)
    C[is.na(C)] <- 0
    W <- choose(degree(proj)[proj_actors], 2)
    return(unname(cbind(W, as.integer(W * C))))
  }
}
