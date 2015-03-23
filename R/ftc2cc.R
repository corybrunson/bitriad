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

ftc2cc <-
    function(ftc, S.fn, F.fn, num.denom = FALSE) {
        if(dim(ftc)[1] * dim(ftc)[2] == 0) return(NA)
        S.c <- sum(sapply(1:dim(ftc)[2], function(j) sapply(
            1:dim(ftc)[1], function(i) {
                if(ftc[i, j] == 0) 0 else
                    S.fn(indexPartition(i - 1), j - 1) * ftc[i, j]})))
        F.c <- sum(sapply(1:dim(ftc)[2], function(j) sapply(
            1:dim(ftc)[1], function(i) {
                if(ftc[i, j] == 0) 0 else
                    F.fn(indexPartition(i - 1), j - 1) * ftc[i, j]})))
        return(if(num.denom) c(S.c, S.c + F.c) else S.c / (S.c + F.c))}
