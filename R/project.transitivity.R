#' Affiliation network clustering coefficients
#'
#' Each clustering coefficient can be defined as the proportion of "wedges" that
#' are "closed", for suitable definitions of both terms. The main function,
#' transitivity.an, calls one of the wedge functions and computes the
#' global or local clustering coefficient of the given affiliation network,
#' and if the local, then at the given nodes. (`project.transitivity`
#' cheats by using the native `transitivity` but produces output consistent
#' with the other variants of `transitivity.an`.)
#' @param bigraph An affiliation network.
#' @param type The type of clustering coefficient (defaults to "global")
#' @param vids A subset of actor node ids at which to evaluate the local
#' clustering coefficient.
#' @export

project.transitivity <-
    function(
        bigraph, type = "global",
        vids = which(!V(bigraph)$type)
    ) {
        if(vcount(bigraph) == 0) {
            if(type == 'global') {
                return(NaN)
            } else if(type == 'local') {
                return(NULL)
            } else return(matrix(NA, nr = 0, nc = 2))
        }
        stopifnot(all(!V(bigraph)$type[vids]))
        graph <- actor.projection(bigraph)
        proj.vids <- which(which(!V(bigraph)$type) %in% vids)
        stopifnot(length(proj.vids) == length(vids))
        if(type == 'global') {
            return(transitivity(graph, type = 'global'))
        }
        C <- transitivity(graph, type = 'local', vids = proj.vids)
        if(type == 'local') return(C)
        C[is.na(C)] <- 0
        W <- choose(degree(graph)[proj.vids], 2)
        unname(cbind(W, W * C))
    }
