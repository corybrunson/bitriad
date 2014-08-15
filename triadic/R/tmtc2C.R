#' Global clustering coefficients from the two-mode triad census
#' 
#' Each global clustering coefficient can be recovered from the two-mode triad
#' census. the first function provides a framework for this calculation, and the
#' following call that framework for specific clustering coefficients.
#' @param tc A two-mode triad census (a matrix)
#' @param S.fn The "success" function, the closed wedge count for a triad
#' @param F.fn The "failure" function, the unclosed wedge count for a triad
#' @param num.denom Whether to return the numerator and denominator of the
#' clustering coefficient as a length-2 vector.
#' @export

tmtc2C <-
function(tc, num.denom = FALSE, by.tri = FALSE) tmtc2cc(
        tc,
        function(L, w) ifelse(by.tri, 1, 3) * ((L[3] > 0) | (w > 0)),
        function(L, w) ((L[2] > 0) & (L[3] == 0) & (w == 0)),
        num.denom = num.denom)
