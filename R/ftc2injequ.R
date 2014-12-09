#' Global clustering coefficients from the triad census
#' 
#' Each global clustering coefficient can be recovered from the full triad
#' census. the first function provides a framework for this calculation, and the
#' following call that framework for specific clustering coefficients.
#' @param ftc A full triad census (a matrix)
#' @param S.fn The "success" function, the closed wedge count for a triad
#' @param F.fn The "failure" function, the unclosed wedge count for a triad
#' @param num.denom Whether to return the numerator and denominator of the
#' clustering coefficient as a length-2 vector.
#' @export

ftc2injequ <-
    function(ftc, num.denom = FALSE, by.tri = FALSE) ftc2cc(
        ftc,
        function(L, w) ifelse(
            by.tri,
            L[1] * L[2] * L[3] +
                (L[1] * L[2] + L[1] * L[3] + L[2] * L[3]) * w +
                sum(L) * w * (w - 1) +
                w * (w - 1) * (w - 2),
            L[1] * L[2] * (L[3] + w > 0) + L[1] * L[3] + L[2] * L[3] +
                L[1] * w * (L[2] > 0 | w > 1) + L[1] * w * (L[3] > 0 | w > 1) +
                L[2] * w + L[2] * w * (L[3] > 0 | w > 1) +
                2 * L[3] * w +
                2 * choose(w, 2) * max(3 * (w > 2), length(which(L > 0)))),
        function(L, w) L[1] * L[2] * (L[3] + w == 0) +
            L[1] * (L[2] == 0 & w == 1) + L[1] * (L[3] == 0 & w == 1) +
            L[2] * (L[3] == 0 & w == 1) +
            2 * choose(w, 2) * min(3 * (w == 2), length(which(L == 0))),
        num.denom = num.denom)
ftc2opsahl <- ftc2injequ
