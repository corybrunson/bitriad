#' @title Global triad closure from a triad census
#'   
#' @description Given a triad census of a suitable scheme, calculate a global 
#'   measure of triad closure for the associated affiliation network.
#'   
#' @details Each global measure of triad closure can be recovered from the full 
#'   triad census, and some can be recovered from smaller censuses. This 
#'   function verifies that a given census is sufficient to recover a given 
#'   measure of triad closure and, if it is, returns its value.
#'   
#' @template triadcensus
#' @template triadclosure
#'   
#' @name triad_closure_from_census
#' @family triad census functions
#' @family triad closure functions
#' @param census Numeric matrix or vector; an affiliation network triad census. 
#'   It is treated as binary or simple if its dimensons are 4-by-2 or 4-by-1, 
#'   respectively, unless otherwise specified by \code{scheme}; otherwise it is 
#'   treated as full.
#' @param scheme Character; the type of triad census provided, matched to 
#'   \code{"full"}, \code{"difference"} (also \code{"uniformity"}), 
#'   \code{"binary"} (also \code{"structural"}), or \code{"simple"}.
#' @param alcove,wedge,maps,congruence Choice of alcove, wedge, maps, and 
#'   congruence (see Details).
#' @param measure Character; the measure of triad closure (matched to 
#'   "classical", "watts_strogatz", "twomode", "opsahl", "unconnected", 
#'   "liebig_rao_0", "completely_connected", "liebig_rao_3", "exclusive", 
#'   "allact", "indequ", "indstr", "injact", "injequ", or "injstr"). Overrides 
#'   \code{alcove}, \code{wedge}, \code{maps}, and \code{congruence}.
#' @param open.fun,closed.fun Functions to calculate the open and closed 
#'   wedge count for a triad (when \code{scheme} is \code{"full"}) or a triad
#'   census (otherwise), in order to calculate a custom measure of triad 
#'   closure. Override \code{measure}.
#' @param counts Logical; whether to return open and closed wedge counts 
#'   instead of the quotient.
#' @param ... Arguments passed from deprecated functions to their replacements.
#' @export
triad_closure_from_census <- function(
  census, scheme = NULL,
  alcove = 0, wedge = 0, maps = 0, congruence = 0,
  measure = NULL,
  open.fun = NULL, closed.fun = NULL,
  counts = FALSE
) {
  # put into matrix form (single column if vector)
  census <- as.matrix(census)
  # identify the census scheme
  scheme <- census_scheme(census = census, scheme = scheme)
  
  # replace a named measure with the corresponding specifications
  if (!is.null(measure)) {
    if (!is.null(open.fun) | !is.null(closed.fun)) {
      stop("Provide either 'measure' or both 'open.fun' and 'closed.fun', ",
           "but not both.")
    }
    measure <- match.arg(measure, c(
      "classical", "watts_strogatz",
      "twomode", "opsahl",
      "unconnected", "liebig_rao_0",
      #"sparsely_connected", "liebig_rao_1",
      #"highly_connected", "liebig_rao_2",
      "completely_connected", "liebig_rao_3",
      "exclusive",
      "allequ", "allstr", "allact",
      "injequ", "injstr", "injact",
      "indequ", "indstr", "indequ"
    ))
    #mc <- measure_codes[[measure]]
    funs <- get(paste0("wedges_", measure, "_from_", scheme, "_census"))
    return(triad_closure_from_census(
      census = census, scheme = scheme,
      #alcove = mc[1], wedge = mc[2], maps = mc[3], congruence = mc[4],
      measure = NULL,
      open.fun = funs$open, closed.fun = funs$closed,
      counts = counts
    ))
  }
  
  # calculate wedges from specific census scheme
  triad_closure_from_census_fun <-
    get(paste0("triad_closure_from_", scheme, "_census"))
  triad_closure_from_census_fun(
    census = census,
    alcove = alcove, wedge = wedge, maps = maps, congruence = congruence,
    open.fun = open.fun, closed.fun = closed.fun,
    counts = counts
  )
}

#' @rdname triad_closure_from_census
#' @export
triad_closure_from_simple_census <- function(
  census,
  alcove = 0, wedge = 0, maps = 0, congruence = 0,
  open.fun = NULL, closed.fun = NULL,
  counts = FALSE
) {
  if (!is.null(open.fun) & !is.null(closed.fun)) {
    wedgecount <- c(wedges = open.fun(census), closed = closed.fun(census))
  } else {
    if (!is.null(open.fun)) warning("Ignoring 'open.fun'.")
    if (!is.null(closed.fun)) warning("Ignoring 'closed.fun'.")
    if (alcove != 0 | wedge != 0 | maps != 0 | congruence != 2) {
      stop("Specified wedges cannot be recovered from a simple census.")
    }
    wedgecount <- as.vector(census)[c(3, 4), 1]
  }
  if (counts) {
    return(wedgecount)
  } else {
    return(unname(wedgecount[["closed"]] / sum(wedgecount)))
  }
}

#' @rdname triad_closure_from_census
#' @export
triad_closure_from_binary_census <- function(
  census,
  alcove = 0, wedge = 0, maps = 0, congruence = 0,
  open.fun = NULL, closed.fun = NULL,
  counts = FALSE
) {
  if (!is.null(open.fun) & !is.null(closed.fun)) {
    wedgecount <- c(wedges = open.fun(census), closed = closed.fun(census))
  } else {
    stop("Parameterized functions have not yet been implemented.")
    if (!is.null(open.fun)) warning("Ignoring 'open.fun'.")
    if (!is.null(closed.fun)) warning("Ignoring 'closed.fun'.")
    wedgecount <- wedges_from_binary_census_C(
      census,
      alcove = alcove, wedge = wedge, maps = maps, congruence = congruence
    )
  }
  if (counts) {
    return(wedgecount)
  } else {
    return(unname(wedgecount[["closed"]] / sum(wedgecount)))
  }
}

#' @rdname triad_closure_from_census
#' @export
triad_closure_from_difference_census <- function(
  census,
  alcove = 0, wedge = 0, maps = 0, congruence = 0,
  open.fun = NULL, closed.fun = NULL,
  counts = FALSE
) {
  if (!is.null(open.fun) & !is.null(closed.fun)) {
    wedgecount <- c(wedges = open.fun(census), closed = closed.fun(census))
  } else {
    stop("Parameterized functions have not yet been implemented.")
  }
  if (counts) {
    return(wedgecount)
  } else {
    return(unname(wedgecount[["closed"]] / sum(wedgecount)))
  }
}

#' @rdname triad_closure_from_census
#' @export
triad_closure_from_full_census <- function(
  census,
  alcove = 0, wedge = 0, maps = 0, congruence = 0,
  open.fun = NULL, closed.fun = NULL,
  counts = FALSE
) {
  if (is.null(open.fun) | is.null(closed.fun)) {
    if (!is.null(open.fun)) warning("Ignoring 'open.fun'.")
    if (!is.null(closed.fun)) warning("Ignoring 'closed.fun'.")
    suffix <- paste0(
      "x", alcove,
      "w", wedge,
      "m", maps,
      "c", congruence
    )
    funs <- get(paste0("wedges_", suffix, "_from_full_census"))
    open.fun <- funs$open
    closed.fun <- funs$closed
  }
  wedgecount <- wedges_from_full_census(census, open.fun, closed.fun)
  if (counts) {
    return(wedgecount)
  } else {
    return(unname(wedgecount[["closed"]] / sum(wedgecount)))
  }
}

#' @rdname triad_closure_from_census
#' @export
wedges_from_full_census <- function(
  census, open.fun, closed.fun
) {
  if (prod(dim(census)) == 0) return(NaN)
  opencount <- sum(sapply(1:ncol(census) - 1, function(w) {
    apply(sapply(1:nrow(census) - 1, index_partition), 2, function(lambda) {
      open.fun(lambda, w)
    })
  }) * census)
  closedcount <- sum(sapply(1:ncol(census) - 1, function(w) {
    apply(sapply(1:nrow(census) - 1, index_partition), 2, function(lambda) {
      closed.fun(lambda, w)
    })
  }) * census)
  #opencount <- sum(outer(1:nrow(census), 1:ncol(census), function(i, j) {
  #  open.fun(sapply(i - 1, index_partition), j - 1)
  #}) * census)
  #closedcount <- sum(outer(1:nrow(census), 1:ncol(census), function(i, j) {
  #  closed.fun(sapply(i - 1, index_partition), j - 1)
  #}) * census)
  c(open = opencount, closed = closedcount)
}

#' @rdname triad_closure_from_census
#' @export
wedges_from_census <- function(...) {
  .Deprecated("wedges_from_full_census")
  wedges_from_full_census(...)
}

#' @rdname triad_closure_from_census
#' @export
wedgecount_census <- function(...) {
  .Deprecated("wedges_from_full_census")
  wedges_from_full_census(...)
}

#' @rdname triad_closure_from_census
#' @export
wedgecount.census <- function(...) {
  .Deprecated("wedges_from_full_census")
  wedges_from_full_census(...)
}

wedges_x0w0m0c0_from_full_census <- list(
  open = function(L, w) L[1] * L[2] * (L[3] == 0 & w == 0),
  closed = function(L, w)
    L[1] * L[2] * (L[3] > 0 | w > 0) +  # p,x,q,y,r;z|w
    L[2] * L[3] +                       # q,y,r,z,p;x
    L[1] * L[3] +                       # r,z,p,x,q;y
    2 * sum(L) * w +                    # p1,x1,p2,w,p3 & p1,w,p2,x2,p3
    3 * w ^ 2                           # p1,w1,p2,w2,p3
)
wedges_allequ_from_full_census <- wedges_x0w0m0c0_from_full_census

wedges_x0w0m0c1_from_full_census <- list(
  open = function(L, w) (L[2] > 0) * (L[3] == 0 & w == 0),
  closed = function(L, w)
    (L[2] > 0) * (L[3] > 0 | w > 0) +  # x,y;z|w
    2 * (L[3] > 0) +                   # x,z;y & y,z;x
    2 * sum(L > 0) * (w > 0) +         # x1,w;w & w,x1;w
    3 * (w > 0)                        # w,w;w
)
wedges_allstr_from_full_census <- wedges_x0w0m0c1_from_full_census

wedges_x0w0m0c2_from_full_census <- list(
  open = function(L, w) ((L[2] > 0) & (L[3] == 0) & (w == 0)),
  closed = function(L, w) 3 * ((L[3] > 0) | (w > 0))
)
wedges_watts_strogatz_from_full_census <- wedges_x0w0m0c2_from_full_census
wedges_classical_from_full_census <- wedges_x0w0m0c2_from_full_census
wedges_allact_from_full_census <- wedges_x0w0m0c2_from_full_census

wedges_x0w0m1c0_from_full_census <- list(
  open = function(L, w) {
    L[1] * L[2] * (L[3] + w == 0) +
      L[1] * (L[2] == 0 & w == 1) + L[1] * (L[3] == 0 & w == 1) +
      L[2] * (L[3] == 0 & w == 1) +
      2 * choose(w, 2) * min(3 * (w == 2), length(which(L == 0)))
  },
  closed = function(L, w) {
    L[1] * L[2] * (L[3] + w > 0) + L[1] * L[3] + L[2] * L[3] +
      L[1] * w * (L[2] > 0 | w > 1) + L[1] * w * (L[3] > 0 | w > 1) +
      L[2] * w + L[2] * w * (L[3] > 0 | w > 1) +
      2 * L[3] * w +
      2 * choose(w, 2) * max(3 * (w > 2), length(which(L > 0)))
  }
)
wedges_opsahl_from_full_census <- wedges_x0w0m1c0_from_full_census
wedges_twomode_from_full_census <- wedges_x0w0m1c0_from_full_census
wedges_injequ_from_full_census <- wedges_x0w0m1c0_from_full_census

wedges_x0w0m1c1_from_full_census <- list(
  open = function(L, w) {
    (L[3] == 0) * ((L[2] > 0) * (w == 0) +
                     (sum(L[1:2] > 0) * (w == 1)) +
                     (w == 2)) +
      (L[2] == 0) * ((L[1] > 0) * (w == 1) + (w == 2)) +
      (L[1] == 0) * (w == 2)
  },
  closed = function(L, w) {
    (w == 0) * (3 * (L[3] > 0)) +
      (w == 1) * (3 * (L[2] > 0) + 6 * (L[3] > 0)) +
      (w == 2) * (3 * (L[1] > 0) + 4 * (L[2] > 0) + 5 * (L[3] > 0)) +
      (w >= 3) * (3 + 2 * (L[1] > 0) + 3 * (L[2] > 0) +
                    4 * (L[3] > 0))
  }
)
wedges_injstr_from_full_census <- wedges_x0w0m1c1_from_full_census

wedges_x0w0m1c2_from_full_census <- list(
  open = function(L, w) (L[2] > 0 & L[3] == 0 & w == 0) +
    2 * (L[1] > 0 & L[2] == 0 & w == 1) +
    3 * (L[1] == 0 & w == 2),
  closed = function(L, w) 3 * (length(which(L > 0)) + w > 2)
)
wedges_injact_from_full_census <- wedges_x0w0m1c2_from_full_census

wedges_x0w0m2c0_from_full_census <- list(
  open = function(L, w) L[1] * L[2] * (L[3] == 0),
  closed = function(L, w) if (L[3] == 0) 0 else
    L[1] * L[2] + L[2] * L[3] + L[1] * L[3]
)
wedges_liebig_rao_0_from_full_census <- wedges_x0w0m2c0_from_full_census
wedges_unconnected_from_full_census <- wedges_x0w0m2c0_from_full_census
wedges_indequ_from_full_census <- wedges_x0w0m2c0_from_full_census

wedges_x0w0m2c1_from_full_census <- list(
  open = function(L, w) ((L[2] > 0) & (L[3] == 0)),
  closed = function(L, w) 3 * (L[3] > 0)
)
wedges_exclusive_from_full_census <- wedges_x0w0m2c1_from_full_census
wedges_indstr_from_full_census <- wedges_x0w0m2c1_from_full_census

wedges_x0w0m2c2_from_full_census <- wedges_x0w0m2c1_from_full_census
wedges_indact_from_full_census <- wedges_x0w0m2c2_from_full_census

#' @rdname triad_closure_from_census
#' @export
triad_closure_from_census_original <- function(
  census, scheme = NULL,
  alcove = 0, wedge = 0, maps = 0, congruence = 0,
  measure,
  open.fun, closed.fun,
  counts = FALSE
) {
  .Deprecated("triad_closure_from_census")
  # put into matrix form (single column if vector)
  census <- as.matrix(census)
  # identify the census scheme
  scheme <- census_scheme(census = census, scheme = scheme)
  # collapse difference censuses to binary censuses (ONLY FOR NOW)
  if (scheme == "difference") {
    message("Not yet implemented for difference census; ",
            "projecting to the binary census.")
    census <- project_census(census, scheme = scheme)$binary
    scheme <- "binary"
  }
  
  # standard names for measures of triad closure
  if (!is.null(measure)) {
    measure <- match.arg(measure, c(
      "classical", "watts_strogatz",
      "twomode", "opsahl",
      "unconnected", "liebig_rao_0",
      "completely_connected", "liebig_rao_3",
      "exclusive",
      "allact",
      "injequ", "injstr", "injact",
      "indequ", "indstr", "indact"
    ))
  }
  if (measure %in% c("classical", "watts_strogatz")) measure <- "allact"
  if (measure %in% c("twomode", "opsahl")) measure <- "injequ"
  if (measure %in% c("unconnected", "liebig_rao_0")) measure <- "indequ"
  if (measure %in% c("exclusive", "indact")) measure <- "indstr"
  
  # simple census can only return classical (Watts-Strogatz) triad closure
  if (scheme == "simple") {
    if (measure == "allact") {
      wedgecount <- c(open = census[3, 1], closed = 3 * census[4, 1])
    } else {
      stop("Specified measure cannot be recovered from a simple census.")
    }
  } else if (scheme == "binary") {
    if (measure == "allact") {
      wedgecount <- c(open = census[3, 1],
                      closed = 3 * (census[4, 1] + sum(census[, 2])))
    } else if (measure == "indstr") {
      wedgecount <- c(open = sum(census[3, ]),
                      closed = 3 * sum(census[4, ]))
    } else {
      stop("Specified measure cannot be recovered from a binary census.")
    }
  } else {
    if (is.null(measure)) {
      wedgecount <- wedges_from_full_census(
        census, open.fun = open.fun, closed.fun = closed.fun
      )
    } else {
      ftcFun <- if (measure == "allact") {
        ftc2allact
      } else if (measure == "indequ") {
        ftc2indequ
      } else if (measure == "indstr") {
        ftc2indstr
      } else if (measure == "injact") {
        ftc2injact
      } else if (measure == "injequ") {
        ftc2injequ
      } else if (measure == "injstr") {
        ftc2injstr
      }
      wedgecount <- ftcFun(census)
    }
  }
  
  # return counts or clustering coefficient
  if (counts) {
    wedgecount
  } else {
    unname(wedgecount[["closed"]] / sum(wedgecount))
  }
}

#' @rdname triad_closure_from_census
#' @export
transitivity_from_census <- function(...) {
  .Deprecated("triad_closure_from_census")
  triad_closure_from_census_original(...)
}

#' @rdname triad_closure_from_census
#' @export
transitivity.census <- function(...) {
  .Deprecated("triad_closure_from_census")
  triad_closure_from_census_original(...)
}

ftc2indequ <- function(census) wedges_from_full_census(
  census,
  function(L, w) L[1] * L[2] * (L[3] == 0),
  function(L, w) if (L[3] == 0) 0 else
    L[1] * L[2] + L[2] * L[3] + L[1] * L[3]
)

ftc2allact <- function(census) wedges_from_full_census(
  census,
  function(L, w) ((L[2] > 0) & (L[3] == 0) & (w == 0)),
  function(L, w) 3 * ((L[3] > 0) | (w > 0))
)

ftc2injequ <- function(census) wedges_from_full_census(
  census,
  function(L, w) {
    L[1] * L[2] * (L[3] + w == 0) +
      L[1] * (L[2] == 0 & w == 1) + L[1] * (L[3] == 0 & w == 1) +
      L[2] * (L[3] == 0 & w == 1) +
      2 * choose(w, 2) * min(3 * (w == 2), length(which(L == 0)))
  },
  function(L, w) {
    L[1] * L[2] * (L[3] + w > 0) + L[1] * L[3] + L[2] * L[3] +
      L[1] * w * (L[2] > 0 | w > 1) + L[1] * w * (L[3] > 0 | w > 1) +
      L[2] * w + L[2] * w * (L[3] > 0 | w > 1) +
      2 * L[3] * w +
      2 * choose(w, 2) * max(3 * (w > 2), length(which(L > 0)))
  }
)

ftc2indstr <- function(census) wedges_from_full_census(
  census,
  function(L, w) ((L[2] > 0) & (L[3] == 0)),
  function(L, w) 3 * (L[3] > 0)
)

ftc2injact <- function(census) wedges_from_full_census(
  census,
  function(L, w) (L[2] > 0 & L[3] == 0 & w == 0) +
    2 * (L[1] > 0 & L[2] == 0 & w == 1) +
    3 * (L[1] == 0 & w == 2),
  function(L, w) 3 * (length(which(L > 0)) + w > 2)
)

ftc2injstr <- function(census) wedges_from_full_census(
  census,
  function(L, w) {
    (L[3] == 0) * ((L[2] > 0) * (w == 0) +
                     (sum(L[1:2] > 0) * (w == 1)) +
                     (w == 2)) +
      (L[2] == 0) * ((L[1] > 0) * (w == 1) + (w == 2)) +
      (L[1] == 0) * (w == 2)
  },
  function(L, w) {
    (w == 0) * (3 * (L[3] > 0)) +
      (w == 1) * (3 * (L[2] > 0) + 6 * (L[3] > 0)) +
      (w == 2) * (3 * (L[1] > 0) + 4 * (L[2] > 0) + 5 * (L[3] > 0)) +
      (w >= 3) * (3 + 2 * (L[1] > 0) + 3 * (L[2] > 0) +
                    4 * (L[3] > 0))
  }
)
