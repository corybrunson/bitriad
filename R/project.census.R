#' Project a triad census
#' 
#' This function inputes an affiliation network triad census of any scheme and
#' returns a list of triad censuses projected from it (not icluding itself). In
#' order of projectability, the census schemes are full (affiliation network),
#' uniformity, structural, and simple.
#' @param census Numeric matrix or vector; the input triad census. It is
#' classified as "uniformity", "structural", or "simple" if its dimensons are
#' 8-by-2, 4-by-2, or 4-by-1, respectively, or unless otherwise specified;
#' otherwise it is classified as "full".
#' @param scheme Character; the type of census (to be matched to "full",
#' "uniformity", "structural", or "simple").
#' @param add.names Logical; whether to label the rows and (where applicable)
#' columns of the projected censuses.
#' @export

project.census <-
    function(census, scheme = NULL, add.names = FALSE) {
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
        if(!is.null(scheme)) {
            if(!(scheme %in% c("full", "uniformity", "structural", "simple")) |
                   (scheme == "uniformity" & !all(cdim == c(8, 2))) |
                   (scheme == "structural" & !all(cdim == c(4, 2))) |
                   (scheme == "simple" & !all(cdim == c(4, 1)))) {
                scheme <- NULL
                warning('Incongruent input; coercing to scheme "full".')
            }
        }
        if(is.null(scheme)) {
            scheme <- if(all(cdim == c(8, 2))) "uniformity" else
                if(all(cdim == c(4, 2))) "structural" else
                    if(all(cdim == c(4, 1))) "simple" else
                        "full"
        }
        # Initiate census list
        censuses <- list()
        # If "full", project to "uniformity"
        if(scheme == "full") {
            census <- ftc2utc(census)
            if(add.names) {
                rownames(census) <- 0:7 # FIX THIS
                colnames(census) <- 0:1
            }
            scheme <- "uniformity"
            censuses <- c(list(uniformity = census), censuses)
        }
        # If "uniformity", project to "structural"
        if(scheme == "uniformity") {
            census <- utc2stc(census)
            if(add.names) {
                rownames(census) <- 0:3
                colnames(census) <- 0:1
            }
            scheme <- "structural"
            censuses <- c(list(structural = census), censuses)
        }
        # If "structural", project to "simple"
        if(scheme == "structural") {
            census <- stc2tc(census)
            if(add.names) {
                names(census) <- 0:3
            }
            scheme <- "simple"
            censuses <- c(list(simple = census), censuses)
        }
        # If "simple", project to "total"
        if(scheme == "simple") {
            censuses <- c(list(total = sum(census)), censuses)
        }
        # Return list
        censuses
    }
