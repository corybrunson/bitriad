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
#' @param census Numeric matrix or vector; an affiliation network triad census. 
#'   It is treated as binary or simple if its dimensons are 4-by-2 or 4-by-1, 
#'   respectively, unless otherwise specified by \code{scheme}; otherwise it is 
#'   treated as full.
#' @param scheme Character; the type of triad census provided, matched to 
#'   \code{"full"}, \code{"binary"} (also \code{"structural"}), 
#'   \code{"difference"} (also \code{"uniformity"}), or \code{"simple"}.
#' @param alcove,wedge,maps,congruence Choice of alcove, wedge, maps, and 
#'   congruence (see Details).
#' @param measure Character; the measure of triad closure (matched to 
#'   "watts.strogatz", "classical", "opsahl", "exclusive", "allact", "indequ", 
#'   "indstr", "injact", "injequ", or "injstr"). Overrides \code{alcove}, 
#'   \code{wedge}, \code{maps}, and \code{congruence}.
#' @param open_fun,closed_fun Functions to calculate the open and closed wedge 
#'   count for a triad, in order to calculate a custom measure of triad closure.
#'   Override \code{measure}.
#' @param counts Logical; whether to return open and closed wedge counts 
#'   \eqn{w_o} and \eqn{w_c} instead of the quotient \eqn{\frac{w_c}{w_o+w_c}}.
#' @seealso triad census functions at \code{\link{triad_census_an}} and triad 
#'   closure functions at \code{\link{triad_closure_an}}.
#' @export
triad_closure_from_census_bottomup <- function(
  census, scheme = NULL,
  alcove = 0, wedge = 0, maps = 0, congruence = 0,
  measure = NULL,
  open_fun, closed_fun,
  counts = FALSE
) {
  # put into matrix form (single column if vector)
  census <- as.matrix(census)
  # identify the census scheme
  scheme <- census_scheme(census = census, scheme = scheme)
  
  # use 'open_fun' and 'closed_fun' if provided
  if (!missing(open_fun) & !missing(closed_fun)) {
    if (!is.null(measure)) {
      warning("'open_fun' and 'closed_fun' are provided, ",
              "so 'measure' argument will be ignored.")
    }
    wedgecount <- wedges_from_census(
      census, closed_fun = closed_fun, open_fun = open_fun
    )
    if (counts) {
      return(wedgecount)
    } else {
      return(unname(wedgecount[2] / (wedgecount[1] + wedgecount[2])))
    }
  }
  # replace named measure with corresponding specifications
  if (!is.null(measure)) {
    mc <- measure_codes[[measure]]
    return(triad_closure_from_census_bottomup(
      census = census, scheme = scheme,
      alcove = mc[1], wedge = mc[2], maps = mc[3], congruence = mc[4],
      counts = counts
    ))
  }
  
  # calculate wedges from triads using C++ function
  wedges_from_census_fun <- get(paste0("wedges_from_", scheme, "_census"))
  wedges_from_census_fun(
    census = census,
    alcove = alcove, wedge = wedge, maps = maps, congruence = congruence,
    counts = counts
  )
}

#' @rdname triad_closure_from_census
#' @export
wedges_from_simple_census <- function(
  census,
  alcove = 0, wedge = 0, maps = 0, congruence = 0,
  counts = FALSE
) {
  if (alcove != 0 | wedge != 0 | maps != 0 | congruence != 2) {
    stop("Specified wedges cannot be recovered from a simple census.")
  }
  wedgecount <- as.vector(census)[c(3, 4), 1]
  if (counts) {
    return(wedgecount)
  } else {
    return(unname(wedgecount[2] / (wedgecount[1] + wedgecount[2])))
  }
}

#' @rdname triad_closure_from_census
#' @export
wedges_from_binary_census <- function(
  census,
  alcove = 0, wedge = 0, maps = 0, congruence = 0,
  counts = FALSE
) {
  wedgecount <- wedges_from_binary_census_C(
    census,
    alcove = alcove, wedge = wedge, maps = maps, congruence = congruence
  )
  if (counts) {
    return(wedgecount)
  } else {
    return(unname(wedgecount[2] / (wedgecount[1] + wedgecount[2])))
  }
}

#' @rdname triad_closure_from_census
#' @export
triad_closure_from_census <- function(
  census,
  scheme = NULL,
  alcove = 0, wedge = 0, maps = 0, congruence = 0,
  measure,
  open_fun, closed_fun,
  counts = FALSE
) {
  # Put into matrix form (single column if vector)
  census <- as.matrix(census)
  # Decide what kind of census it is
  cdim <- dim(census)
  if(!is.null(scheme)) {
    scheme <- match.arg(scheme,
                        c("full",
                          "difference", "uniformity",
                          "binary", "structural",
                          "simple"))
    if((scheme == "uniformity" & !all(cdim == c(8, 2))) |
       (scheme == "structural" & !all(cdim == c(4, 2))) |
       (scheme == "simple" & !all(cdim == c(4, 1)))) {
      scheme <- NULL
      warning('Incongruent input; coercing to scheme "full".')
    }
  }
  # Collapse uniformity censuses to structural censuses (ONLY FOR NOW)
  if(scheme == "uniformity") {
    census <- project_census(census, scheme = scheme)$structural
    scheme <- "structural"
  }
  # Decide what measure of triad closure is desired
  if(!is.null(measure)) {
    measure <- match.arg(measure,
                         c("watts.strogatz", "classical", "opsahl",
                           "exclusive", "allact", "indequ", "indstr",
                           "injact", "injequ", "injstr"))
  }
  if(measure %in% c("watts.strogatz", "classical")) measure <- "allact"
  if(measure == "opsahl") measure <- "injequ"
  if(measure == "exclusive") measure <- "indstr"
  # Simple census can only return classical (Watts-Strogatz) triad closure
  if(scheme == "simple") {
    if(measure == "allact") {
      wedgeCt <- c(open = census[3, 1], closed = 3 * census[4, 1])
    } else {
      stop("Triad closure measure unrecoverable from census scheme")
    }
  } else if(scheme == "structural") {
    if(measure == "allact") {
      wedgeCt <- c(open = census[3, 1],
                   closed = 3 * (census[4, 1] + sum(census[, 2])))
    } else if(measure == "indstr") {
      wedgeCt <- c(open = sum(census[3, ]),
                   closed = 3 * sum(census[4, ]))
    } else {
      stop("Triad closure measure unrecoverable from census scheme")
    }
  } else {
    if(is.null(measure)) {
      wedgeCt <- wedges_from_census(
        census, closed_fun = closed_fun, open_fun = open_fun
      )
    } else {
      ftcFun <- if(measure == "allact") {
        ftc2allact
      } else if(measure == "indequ") {
        ftc2indequ
      } else if(measure == "indstr") {
        ftc2indstr
      } else if(measure == "injact") {
        ftc2injact
      } else if(measure == "injequ") {
        ftc2injequ
      } else if(measure == "injstr") {
        ftc2injstr
      }
      wedgeCt <- ftcFun(census)
    }
  }
  # Return counts or clustering coefficient
  if (counts) {
    wedgeCt
  } else {
    unname(wedgeCt[2] / (wedgeCt[1] + wedgeCt[2]))
  }
}

#' @rdname triad_closure_from_census
#' @export
transitivity_from_census <- triad_closure_from_census

#' @rdname triad_closure_from_census
#' @export
transitivity.census <- triad_closure_from_census

#' @rdname triad_closure_from_census
ftc2indequ <- function(census) wedges_from_census(
  census,
  function(L, w) if(L[3] == 0) 0 else
    L[1] * L[2] + L[2] * L[3] + L[1] * L[3],
  function(L, w) L[1] * L[2] * (L[3] == 0)
)

#' @rdname triad_closure_from_census
ftc2allact <- function(census) wedges_from_census(
  census,
  function(L, w) 3 * ((L[3] > 0) | (w > 0)),
  function(L, w) ((L[2] > 0) & (L[3] == 0) & (w == 0))
)

#' @rdname triad_closure_from_census
ftc2injequ <- function(census) wedges_from_census(
  census,
  function(L, w) {
    L[1] * L[2] * (L[3] + w > 0) + L[1] * L[3] + L[2] * L[3] +
      L[1] * w * (L[2] > 0 | w > 1) + L[1] * w * (L[3] > 0 | w > 1) +
      L[2] * w + L[2] * w * (L[3] > 0 | w > 1) +
      2 * L[3] * w +
      2 * choose(w, 2) * max(3 * (w > 2), length(which(L > 0)))
  },
  function(L, w) {
    L[1] * L[2] * (L[3] + w == 0) +
      L[1] * (L[2] == 0 & w == 1) + L[1] * (L[3] == 0 & w == 1) +
      L[2] * (L[3] == 0 & w == 1) +
      2 * choose(w, 2) * min(3 * (w == 2), length(which(L == 0)))
  }
)

#' @rdname triad_closure_from_census
ftc2indstr <- function(census) wedges_from_census(
  census,
  function(L, w) 3 * (L[3] > 0),
  function(L, w) ((L[2] > 0) & (L[3] == 0))
)

#' @rdname triad_closure_from_census
ftc2injact <- function(census) wedges_from_census(
  census,
  function(L, w) 3 * (length(which(L > 0)) + w > 2),
  function(L, w) (L[2] > 0 & L[3] == 0 & w == 0) +
    2 * (L[1] > 0 & L[2] == 0 & w == 1) +
    3 * (L[1] == 0 & w == 2)
)

#' @rdname triad_closure_from_census
ftc2injstr <- function(census) wedges_from_census(
  census,
  function(L, w) {
    (w == 0) * (3 * (L[3] > 0)) +
      (w == 1) * (3 * (L[2] > 0) + 6 * (L[3] > 0)) +
      (w == 2) * (3 * (L[1] > 0) + 4 * (L[2] > 0) + 5 * (L[3] > 0)) +
      (w >= 3) * (3 + 2 * (L[1] > 0) + 3 * (L[2] > 0) +
                    4 * (L[3] > 0))
  },
  function(L, w) {
    (L[3] == 0) * ((L[2] > 0) * (w == 0) +
                     (sum(L[1:2] > 0) * (w == 1)) +
                     (w == 2)) +
      (L[2] == 0) * ((L[1] > 0) * (w == 1) + (w == 2)) +
      (L[1] == 0) * (w == 2)
  }
)

#' @rdname triad_closure_from_census
#' @export
wedges_from_census <-
  function(census, closed_fun, open_fun) {
    if(dim(census)[1] * dim(census)[2] == 0) return(NaN)
    closedCt <- sum(sapply(1:dim(census)[2], function(j) sapply(
      1:dim(census)[1], function(i) {
        if(census[i, j] == 0) {
          0
        } else {
          closed_fun(index_partition(i - 1), j - 1) * census[i, j]
        }
      })))
    openCt <- sum(sapply(1:dim(census)[2], function(j) sapply(
      1:dim(census)[1], function(i) {
        if(census[i, j] == 0) {
          0
        } else {
          open_fun(index_partition(i - 1), j - 1) * census[i, j]
        }
      })))
    c(open = openCt, closed = closedCt)
  }

#' @rdname triad_closure_from_census
#' @export
wedgecount_census <- wedges_from_census

#' @rdname triad_closure_from_census
#' @export
wedgecount.census <- wedges_from_census

measure_codes <- list(
  classical = c(0, 0, 0, 2),
  projection = c(0, 0, 0, 2),
  watts_strogatz = c(0, 0, 0, 2),
  twomode = c(0, 0, 1, 0),
  opsahl = c(0, 0, 1, 0),
  unconnected = c(0, 0, 2, 0),
  liebig_rao_0 = c(0, 0, 2, 0),
  completely_connected = c(3, 2, 2, 0),
  liebig_rao_3 = c(3, 2, 2, 0),
  exclusive = c(0, 0, 2, 1)
)
