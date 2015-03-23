#' Project a triad census
#' 
#' This function inputes a triad census of any scheme and returns a list of
#' triad censuses projected from it (not icluding itself).
#' @param census The input triad census. It is classified as "uniformity",
#' "structural", or "simple" if its dimensons are 8-by-2, 4-by-2, or 4-by-1,
#' respectively, or unless otherwise specified; otherwise it is classified as
#' "full".
#' @param scheme The type of census the inputed census should be taken to be.
#' @export

census.projection <-
    function(census, scheme = NULL) {
        # Put into matrix form (single column if vector)
        census <- as.matrix(census)
        # Decide what kind of census it is
        cdim <- dim(census)
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
            scheme <- "uniformity"
            censuses <- c(list(uniformity = census), censuses)
        }
        # If "uniformity", project to "structural"
        if(scheme == "uniformity") {
            census <- utc2stc(census)
            scheme <- "structural"
            censuses <- c(list(structural = census), censuses)
        }
        # If "structural", project to "simple"
        if(scheme == "structural") {
            census <- stc2tc(census)
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
