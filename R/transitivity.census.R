#' Global transitivity from a triad census
#' 
#' Each global clustering coefficient can be recovered from the full triad
#' census, and some from smaller censuses. This function determines the type of
#' census being inputed and computes the desired measure of global transitivity,
#' if possible, otherwise throwing an error.
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
#' @param stat Character; the form of the statistic (matched to "clustering" or
#' "transitivity"; defaults to "clust"; ignored if `counts` is `TRUE`).
#' @param counts Logical; whether to return open and closed wedge counts instead
#' of a ratio statistic (if `TRUE`, overrides `stat`; defaults to `FALSE`).
#' @export

census.transitivity <-
    function(
        census, flavor, scheme = NULL, openFun, closedFun,
        stat = "clust", counts = FALSE
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
            census <- project.census(census, scheme = scheme)$structural
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
        # Decide what statistic is desired
        stat <- match.arg(stat, c("clustering", "transitivity"))
        # Throw error if transitivity ratio is not meaningful
        if(!(flavor == "indstr" | substr(flavor, 4, 6) == "act") &
               stat == "transitivity") {
            stop("Statistic is not meaningful for transitivity flavor")
        }
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
                wedgeCt <- wedgecount.census(
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
        # Return counts or statistic as desired
        if(counts) {
            wedgeCt
        } else if(stat == "clustering") {
            wedgeCt$closed / (wedgeCt$open + wedgeCt$closed)
        } else {
            wedgeCt$closed / (3 * wedgeCt$open + wedgeCt$closed)
        }
    }
