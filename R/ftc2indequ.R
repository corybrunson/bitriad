#' Global clustering coefficients from the triad census
#' 
#' @param census Numeric matrix; a full triad census

ftc2indequ <-
    function(census) wedgecount.census(
        census,
        function(L, w) if(L[3] == 0) 0 else
            L[1] * L[2] + L[2] * L[3] + L[1] * L[3],
        function(L, w) L[1] * L[2] * (L[3] == 0)
    )
