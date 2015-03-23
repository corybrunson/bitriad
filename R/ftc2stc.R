#' Full triad census to structural triad census
#' 
#' @param tc A full triad census (not necessarily labeled)

ftc2stc <-
    function(tc) {
        # Trivial cases
        if(sum(tc) == 0) return(matrix(0, nr = 4, nc = 2))
        if(all(dim(tc) == 1)) return(matrix(c(tc[1, 1], rep(0, 7)),
                                            nr = 4, nc = 2))
        # No. edges (1, 2, or 3) induced by each nonempty lambda with w = 0
        pw.counts <- sapply(1:(dim(tc)[1] - 1), function(i) {
            length(which(indexPartition(i) > 0))
        })
        # Which rows connect 1, 2, and 3 pairs
        wh <- lapply(1:3, function(i) which(pw.counts == i) + 1)
        matrix(c(
            # Empty triads
            tc[1, 1],
            # No triad event; 1, 2, and 3 pairs connected
            sum(tc[wh[[1]], 1]),
            sum(tc[wh[[2]], 1]),
            sum(tc[wh[[3]], 1]),
            # Committees with no pairwise events
            sum(tc[1, 2:dim(tc)[2]]),
            # Triad event; 1, 2, and 3 pairs connected
            sum(tc[wh[[1]], 2:dim(tc)[2]]),
            sum(tc[wh[[2]], 2:dim(tc)[2]]),
            sum(tc[wh[[3]], 2:dim(tc)[2]])
        ), nr = 4, nc = 2)
    }
