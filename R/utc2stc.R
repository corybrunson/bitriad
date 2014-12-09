#' Uniformity triad census to structural triad census
#' 
#' @param tc A uniformity triad census (not necessarily labeled)
#' @export

utc2stc <-
    function(tc) {
        return(matrix(c(
            tc[1, 1],
            tc[2, 1],
            sum(tc[3:4, 1]),
            sum(tc[5:8, 1]),
            tc[1, 2],
            tc[2, 2],
            sum(tc[3:4, 2]),
            sum(tc[5:8, 2])
        ), nr = 4, nc = 2))
    }
