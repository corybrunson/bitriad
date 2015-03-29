#' Global clustering coefficients from the triad census
#' 
#' @param census Numeric matrix; a full triad census

ftc2allact <-
    function(census) wedgecount.census(
        census,
        function(L, w) 3 * ((L[3] > 0) | (w > 0)),
        function(L, w) ((L[2] > 0) & (L[3] == 0) & (w == 0))
    )
