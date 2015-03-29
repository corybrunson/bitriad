#' Global clustering coefficients from the triad census
#' 
#' @param census Numeric matrix; a full triad census

ftc2injstr <-
    function(census) wedgecount.census(
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
