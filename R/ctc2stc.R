#' Cooperativity triad census to simple triad census
#' 
#' @param ctc A cooperativity two-mode triad census (not necessarily labeled)
#' @export

ctc2stc <-
    function(ctc) {
        return(c(
            ctc[1:3, 1],
            sum(c(ctc[4, 1], ctc[1:4, 2]))
        ))
    }
