#' Global clustering coefficients from the full triad census
#' 
#' Each global clustering coefficient can be recovered from the full triad
#' census. the first function provides a framework for this calculation, and the
#' following call that framework for specific clustering coefficients.
#' @param census Numeric matrix; a full triad census
#' @param openFun The open wedge count for a triad
#' @param closedFun The closed wedge count for a triad
#' @export

wedgecount.census <-
    function(census, closedFun, openFun) {
        if(dim(census)[1] * dim(census)[2] == 0) return(NaN)
        closedCt <- sum(sapply(1:dim(census)[2], function(j) sapply(
            1:dim(census)[1], function(i) {
                if(census[i, j] == 0) {
                    0
                } else {
                    closedFun(indexPartition(i - 1), j - 1) * census[i, j]
                }
            })))
        openCt <- sum(sapply(1:dim(census)[2], function(j) sapply(
            1:dim(census)[1], function(i) {
                if(census[i, j] == 0) {
                    0
                } else {
                    openFun(indexPartition(i - 1), j - 1) * census[i, j]
                }
            })))
        c(open = openCt, closed = closedCt)
    }
