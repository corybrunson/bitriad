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

tmtc2cc <-
function(tc, S.fn, F.fn, num.denom = FALSE) {
    if(dim(tc)[1] * dim(tc)[2] == 0) return(NA)
    S.c <- sum(sapply(1:dim(tc)[2], function(j) sapply(
        1:dim(tc)[1], function(i) {
            if(tc[i, j] == 0) 0 else
                S.fn(index.partition(3, i - 1), j - 1) * tc[i, j]})))
    F.c <- sum(sapply(1:dim(tc)[2], function(j) sapply(
        1:dim(tc)[1], function(i) {
            if(tc[i, j] == 0) 0 else
                F.fn(index.partition(3, i - 1), j - 1) * tc[i, j]})))
    return(if(num.denom) c(S.c, S.c + F.c) else S.c / (S.c + F.c))}
