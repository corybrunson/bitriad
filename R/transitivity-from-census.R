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
#' @name triad_closure_from_census
#' @param census Numeric matrix or vector; an affiliation network triad census. 
#'   It is treated as binary or simple if its dimensons are 4-by-2 or 4-by-1, 
#'   respectively, unless otherwise specified by \code{scheme}; otherwise it is 
#'   treated as full.
#' @param measure Character; the type of triad closure (matched to 
#'   "watts.strogatz", "classical", "opsahl", "exclusive", "allact", "indequ", 
#'   "indstr", "injact", "injequ", or "injstr")
#' @param scheme Character; the type of triad census to calculate, matched to 
#'   \code{"full"}, \code{"binary"} (equivalently, \code{"structural"}), or 
#'   \code{"simple"}.
#' @param openFun The open wedge count for a triad (ignored if \code{measure} is
#'   not \code{NULL}).
#' @param closedFun The closed wedge count for a triad (ignored if
#'   \code{measure} is not \code{NULL}).
#' @param counts Logical; whether to return open and closed wedge counts instead
#'   of a ratio statistic.
#' @export
triad_closure_from_census <- function(
    census,
    measure,
    scheme = NULL,
    openFun, closedFun,
    counts = FALSE
  ) {
    # Put into matrix form (single column if vector)
    census <- as.matrix(census)
    # Decide what kind of census it is
    cdim <- dim(census)
    if(!is.null(scheme)) {
      scheme <- match.arg(scheme,
                          c("full", "uniformity", "structural", "simple"))
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
        wedgeCt <- wedgecount_census(
          census, closedFun = closedFun, openFun = openFun
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
    if(counts) {
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
ftc2indequ <- function(census) wedgecount_census(
    census,
    function(L, w) if(L[3] == 0) 0 else
      L[1] * L[2] + L[2] * L[3] + L[1] * L[3],
    function(L, w) L[1] * L[2] * (L[3] == 0)
  )

#' @rdname triad_closure_from_census
ftc2allact <- function(census) wedgecount_census(
    census,
    function(L, w) 3 * ((L[3] > 0) | (w > 0)),
    function(L, w) ((L[2] > 0) & (L[3] == 0) & (w == 0))
  )

#' @rdname triad_closure_from_census
ftc2injequ <- function(census) wedgecount_census(
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
ftc2indstr <- function(census) wedgecount_census(
    census,
    function(L, w) 3 * (L[3] > 0),
    function(L, w) ((L[2] > 0) & (L[3] == 0))
  )

#' @rdname triad_closure_from_census
ftc2injact <- function(census) wedgecount_census(
    census,
    function(L, w) 3 * (length(which(L > 0)) + w > 2),
    function(L, w) (L[2] > 0 & L[3] == 0 & w == 0) +
      2 * (L[1] > 0 & L[2] == 0 & w == 1) +
      3 * (L[1] == 0 & w == 2)
  )

#' @rdname triad_closure_from_census
ftc2injstr <- function(census) wedgecount_census(
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
