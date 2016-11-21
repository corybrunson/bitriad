#' Global transitivity from a triad census
#' 
#' Each global clustering coefficient can be recovered from the full triad
#' census, and some from smaller censuses. This function determines the type of
#' census being inputed and computes the desired measure of global transitivity,
#' if possible, otherwise throwing an error.
#' 
#' @name transitivity_from_census
#' @param census Numeric matrix; the input triad census. It is classified as
#' "uniformity", "structural", or "simple" if its dimensons are 8-by-2, 4-by-2,
#' or 4-by-1, respectively, or unless otherwise specified; otherwise it is
#' classified as "full".
#' @param flavor Character; the type of transitivity (matched to
#' "watts.strogatz", "classical", "opsahl", "exclusive", "allact", "indequ",
#' "indstr", "injact", "injequ", or "injstr")
#' @param scheme Character; the type of census (to be matched to "full",
#' "uniformity", "structural", or "simple").
#' @param openFun The open wedge count for a triad (ignored if `flavor` is not
#' `NULL`).
#' @param closedFun The closed wedge count for a triad (ignored if `flavor` is
#' not `NULL`).
#' @param counts Logical; whether to return open and closed wedge counts instead
#' of a ratio statistic (defaults to `FALSE`).
#' @export
transitivity_from_census <-
  function(
    census, flavor, scheme = NULL, openFun, closedFun, counts = FALSE
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
    # Decide what flavor of transitivity is desired
    if(!is.null(flavor)) {
      flavor <- match.arg(flavor,
                          c("watts.strogatz", "classical", "opsahl",
                            "exclusive", "allact", "indequ", "indstr",
                            "injact", "injequ", "injstr"))
    }
    if(flavor %in% c("watts.strogatz", "classical")) flavor <- "allact"
    if(flavor == "opsahl") flavor <- "injequ"
    if(flavor == "exclusive") flavor <- "indstr"
    # Simple census can only return classical (Watts-Strogatz) transitivity
    if(scheme == "simple") {
      if(flavor == "allact") {
        wedgeCt <- c(open = census[3, 1], closed = 3 * census[4, 1])
      } else {
        stop("Transitivity flavor unrecoverable from census scheme")
      }
    } else if(scheme == "structural") {
      if(flavor == "allact") {
        wedgeCt <- c(open = census[3, 1],
                     closed = 3 * (census[4, 1] + sum(census[, 2])))
      } else if(flavor == "indstr") {
        wedgeCt <- c(open = sum(census[3, ]),
                     closed = 3 * sum(census[4, ]))
      } else {
        stop("Transitivity flavor unrecoverable from census scheme")
      }
    } else {
      if(is.null(flavor)) {
        wedgeCt <- wedgecount_census(
          census, closedFun = closedFun, openFun = openFun
        )
      } else {
        ftcFun <- if(flavor == "allact") {
          ftc2allact
        } else if(flavor == "indequ") {
          ftc2indequ
        } else if(flavor == "indstr") {
          ftc2indstr
        } else if(flavor == "injact") {
          ftc2injact
        } else if(flavor == "injequ") {
          ftc2injequ
        } else if(flavor == "injstr") {
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

#' @rdname transitivity_from_census
ftc2indequ <-
  function(census) wedgecount_census(
    census,
    function(L, w) if(L[3] == 0) 0 else
      L[1] * L[2] + L[2] * L[3] + L[1] * L[3],
    function(L, w) L[1] * L[2] * (L[3] == 0)
  )

#' @rdname transitivity_from_census
ftc2allact <-
  function(census) wedgecount_census(
    census,
    function(L, w) 3 * ((L[3] > 0) | (w > 0)),
    function(L, w) ((L[2] > 0) & (L[3] == 0) & (w == 0))
  )

#' @rdname transitivity_from_census
ftc2injequ <-
  function(census) wedgecount_census(
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

#' @rdname transitivity_from_census
ftc2indstr <-
  function(census) wedgecount_census(
    census,
    function(L, w) 3 * (L[3] > 0),
    function(L, w) ((L[2] > 0) & (L[3] == 0))
  )

#' @rdname transitivity_from_census
ftc2injact <-
  function(census) wedgecount_census(
    census,
    function(L, w) 3 * (length(which(L > 0)) + w > 2),
    function(L, w) (L[2] > 0 & L[3] == 0 & w == 0) +
      2 * (L[1] > 0 & L[2] == 0 & w == 1) +
      3 * (L[1] == 0 & w == 2)
  )

#' @rdname transitivity_from_census
ftc2injstr <-
  function(census) wedgecount_census(
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
