#' Global clustering coefficients from the triad census
#' 
#' @param census Numeric matrix; a full triad census

ftc2injact <-
    function(census) wedgecount.census(
        census,
        function(L, w) 3 * (length(which(L > 0)) + w > 2),
        function(L, w) (L[2] > 0 & L[3] == 0 & w == 0) +
            2 * (L[1] > 0 & L[2] == 0 & w == 1) +
            3 * (L[1] == 0 & w == 2)
    )
