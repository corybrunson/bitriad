#' Triadic closure for dynamic affiliation networks
#' 
#' Given an affiliation network with time-stamped events, compute the proportion
#' of centered triples at which an open wedge exists at some time that is closed
#' at a later time.
#' 
#' @name dynamic_triad_closure
#' @param bigraph An affiliation network with time-stamped events.
#' @param actors A vector of actor nodes in \code{bigraph}.
#' @param type The type of statistic, matched to \code{"global"}, 
#'   \code{"local"}, or \code{"raw"}.
#' @param ... Additional parameters passed to specific functions.
#' @param measure Character; the measure of triad closure, used as the suffix 
#'   \code{*} to \code{triad_closure_*} Matched to \code{"classical"} (also 
#'   \code{"watts_strogatz"}), \code{"twomode"} (also \code{"opsahl"}), 
#'   \code{"unconnected"} (also \code{"liebig_rao_0"}), 
#'   \code{"completely_connected"} (also \code{"liebig_rao_3"}), 
#'   \code{"exclusive"}, or \code{"projection"}.
#' @param memory Numeric; a duration of time after which events are forgotten.
#' @examples
#' data(women_group)
#' dynamic_triad_closure_an(women_group)
#' cbind(
#'   transitivity(actor_projection(women_group), type = "local"),
#'   triad_closure_opsahl(women_group, type = "local"),
#'   triad_closure_exclusive(women_group, type = "local"),
#'   dynamic_triad_closure_an(women_group, type = "local")
#' )
#' @export
dynamic_triad_closure_an <- function(
  bigraph, actors = V(bigraph)[V(bigraph)$type == FALSE],
  type = "global",
  ...,
  measure = NULL
) {
  if (!is_dynamic_an(bigraph)) {
    stop("Not a dynamic affiliation network.")
  }
  type <- match.arg(type, c("global", "local", "raw"))
  if (type == "global" &&
      !setequal(V(bigraph)[V(bigraph)$type == FALSE], V(bigraph)[actors])) {
    warning("Calculating a global statistic on a subset of actors.")
  }
  if (!is.null(measure)) {
    measure <- match.arg(measure, c("classical", "watts_strogatz",
                                    "twomode", "opsahl",
                                    "unconnected", "liebig_rao_0",
                                    "completely_connected", "liebig_rao_3",
                                    "exclusive",
                                    "projection"))
    if (measure == "projection") {
      return(dynamic_triad_closure_projection(bigraph, ..., type = type))
    }
  }
  wedges_fun <- if (!is.null(measure)) {
    get(paste0("dynamic_wedges_", measure))
  } else {
    dynamic_wedges
  }
  wedges <- sapply(actors, function(actor) {
    wc <- wedges_fun(bigraph, actor, ...)$closed
    c(length(wc), sum(wc))
  })
  wedgeReturn(wedges = t(wedges), type = type)
}

#' @rdname dynamic_triad_closure
#' @export
dynamic_transitivity_an <- dynamic_triad_closure_an

#' @rdname dynamic_triad_closure
#' @export
dyn.transitivity.an <- dynamic_triad_closure_an

#' @rdname dynamic_triad_closure
#' @export
dynamic_triad_closure_projection <- function(
  bigraph,
  memory = Inf,
  type = "global"
) {
  
  if (vcount(bigraph) == 0) {
    # Empty resulting data frame
    wedges_dat <- data.frame(
      wedges = c(),
      closed = c()
    )
    # Tailor the output to global, local, or raw
    return(
      if (type == "global") {
        sum(wedges_dat$closed, na.rm = TRUE) /
          sum(wedges_dat$wedges, na.rm = TRUE)
      } else if (type == "local") {
        wedges_dat$closed / wedges_dat$wedges
      } else wedges_dat
    )
  }
  # HOW TO SKIP AHEAD WITHIN A FUNCTION?
  
  if (!("name" %in% vertex_attr_names(bigraph))) {
    stop('Actors need names')
  }
  
  # Make sure events increase with time, and put actors before events
  perm <- order(c(
    which(!V(bigraph)$type),
    which(V(bigraph)$type)[order(V(bigraph)$time[which(V(bigraph)$type)])]
  ))
  bigraph <- permute(bigraph, perm)
  
  # Actor names
  actors <- V(bigraph)$name[which(!V(bigraph)$type)]
  
  # All values of time, in order
  span <- sort(unique(V(bigraph)$time))
  
  # An empty array of wedges with closure counts (to be incremented)
  wedges <- matrix(NA, nrow = 0, ncol = 4)
  
  for (s in span[-1]) {
    
    # Remove events after time t or before time t - memory
    # from bigraph to get bigraph1
    bigraph1 <- delete_vertices(bigraph, which(
      V(bigraph)$type & (V(bigraph)$time > s | V(bigraph)$time < s - memory)
    ))
    
    # Remove events at time t from bigraph1 to get bigraph0
    bigraph0 <- delete_vertices(bigraph1, which(
      V(bigraph1)$type & (V(bigraph1)$time == s)
    ))
    
    # Remove any actors who did not attend any events in bigraph0
    # from both bigraph0 and bigraph1
    missing_actors <- which(
      !V(bigraph0)$type & (degree(bigraph0) == 0)
    )
    bigraph0 <- delete_vertices(bigraph0, missing_actors)
    # bigraph0 is the cumulative bigraph from s - memory to s, exclusive
    bigraph1 <- delete_vertices(bigraph1, missing_actors)
    # bigraph1 includes any new events at time s (but no new actors)
    
    # Check that the actors of bigraph0 and bigraph1 agree
    stopifnot(all(V(bigraph0)$name[!V(bigraph0)$type] ==
                    V(bigraph1)$name[!V(bigraph1)$type]))
    
    # Projections
    proj0 <- actor_projection(bigraph0)
    proj1 <- actor_projection(bigraph1)
    stopifnot(all(V(proj0)$name == V(proj1)$name))
    
    # Local transitivities of proj0
    tr0 <- transitivity(proj0, type = "local")
    
    # Exclude actors with undefined or complete transitivity
    wh.tr0 <- which((!is.na(tr0)) & (tr0 != 1))
    if (length(wh.tr0) == 0) next
    
    # Survey the wedges at these actors
    wedges01 <- do.call(rbind, lapply(wh.tr0, function(v) {
      # Actor's neighbors
      nbhd <- sort(
        setdiff(neighborhood(proj0, order = 1, nodes = v)[[1]], v)
      )
      # Pairs of neighbors
      pairs <- t(matrix(utils::combn(nbhd, 2), nrow = 2))
      # Those that are not connected form wedges...
      pairs <- matrix(pairs[-which(apply(pairs, 1, function(x) {
        are.connected(proj0, v1 = x[1], v2 = x[2])
      })), ], ncol = 2)
      if (nrow(pairs) == 0) return(matrix(NA, ncol = 4, nrow = 0))
      # ...with v at the corner...
      pairs <- cbind(pairs[, 1], unname(v), pairs[, 2])
      # ...that are closed if the end nodes are tied in proj1
      pairs <- cbind(pairs, apply(pairs, 1, function(x) {
        are.connected(proj1, v1 = x[1], v2 = x[3])
      }))
    }))
    if (nrow(wedges01) == 0) next
    
    # Convert IDs in wedges01 from those of proj0 to those of bigraph0
    wedges01[, 1:3] <-
      as.numeric(V(bigraph)[V(proj0)$name[wedges01[, 1:3]]])
    
    # Aggregate these with the existing survey
    wedges <- rbind(wedges, wedges01)
    wedges <- as.matrix(unname(stats::aggregate(
      wedges[, 4],
      by = list(wedges[, 1], wedges[, 2], wedges[, 3]),
      FUN = max  # use `sum` to count closures
    )))
    
  }
  
  # Count the number of wedges and the number that close for each actor
  wedges_dat <- if (nrow(wedges) == 0) {
    data.frame(
      wedges = rep(0, length(actors)),
      closed = rep(0, length(actors))
    )
  } else {
    data.frame(
      wedges = tabulate(wedges[, 2], nbins = length(actors)),
      closed = sapply(1:length(actors), function(v) {
        sum(wedges[which(wedges[, 2] == v), 4])
      })
    )
  }
  rownames(wedges_dat) <- actors
  
  # Tailor the output to global, local, or raw
  if (type == "global") {
    sum(wedges_dat$closed, na.rm = TRUE) /
      sum(wedges_dat$wedges, na.rm = TRUE)
  } else if (type == "local") {
    wedges_dat$closed / wedges_dat$wedges
  } else wedges_dat
  
}
