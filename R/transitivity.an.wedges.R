#' Affiliation network clustering coefficients: wedges method
#' 
#' This method relies on a separate "wedge function" for each statistic. The algorithm calls the appropriate wedge function to run over the necessary wedge centers and return a wedge count matrix, which is returned back into \code{\link{transitivity.an}} for outputting.
#' 
#' @param bigraph An affiliation network; see \code{is.an}.
#' @param vids A subset of actor node ids at which to evaluate the local
#' clustering coefficient.
#' @param wedgeFun The wedge function.
#' @return A two-column matrix of wedge counts and closed wedge counts at the nodes in vids.

transitivity.an.wedges <-
    function(
        bigraph,
        vids = which(!V(bigraph)$type),
        wedgeFun
    ) {
        # Wedge and closed wedge counts at each node
        t(matrix(unlist(lapply(vids, function(v) {
            wedgeFun(bigraph, v)
        })), nr = 2))
    }
