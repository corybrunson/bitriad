#' Full triad census to simple triad census
#' 
#' @param tc A full triad census (not necessarily labeled)
#' @export

ftc2tc <-
    function(tc) {
        # Trivial cases
        if(sum(tc) == 0) return(rep(0, 4))
        if(all(dim(tc) == 1)) return(c(tc[1, 1], 0, 0, 0))
        # Number of edges (1, 2, or 3) induced by each lambda except c(0, 0, 0)
        pw.counts <- sapply(1:(dim(tc)[1] - 1), function(i) {
            length(which(index.partition(i) > 0))
        })
        c(
            # Empty triads all have lambda = c(0, 0, 0), w = 0
            tc[1, 1],
            # Dyads
            sum(tc[which(pw.counts == 1) + 1, 1]),
            # Intransitive wedges
            sum(tc[which(pw.counts == 2) + 1, 1]),
            # Triangles, including all columns w > 0
            sum(tc[which(pw.counts == 3) + 1, 1]) +
                ifelse(dim(tc)[2] == 1, 0, sum(tc[, 2:dim(tc)[2]])))
    }
