#' Uniformity triad census to cooperativity triad census
#' 
#' @param utc A uniformity two-mode triad census (not necessarily labeled)
#' @export

utc2ctc <-
    function(utc) {
        return(matrix(c(
            utc[1, 1],
            utc[2, 1],
            sum(utc[3:4, 1]),
            sum(utc[5:8, 1]),
            utc[1, 2],
            utc[2, 2],
            sum(utc[3:4, 2]),
            sum(utc[5:8, 2])
        ), nr = 4, nc = 2))
    }
