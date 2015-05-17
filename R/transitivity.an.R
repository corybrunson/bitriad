#' Affiliation network clustering coefficients
#' 
#' This function computes a given flavor of transitivity (triadic closure) on a
#' given affiliation network. The calculations are performed locally and can be
#' returned in either of three formats: global (a single statistic), local (a
#' vector of values labeled by actor names), and raw. Each flavor is defined as
#' a proportion of "wedges" that are "closed", for suitable definitions of both
#' terms. The raw format is a 2-column matrix, each row of which gives the
#' number of wedges and the number of closed wedges centered at an actor. The
#' function `transitivity.an` is a shell that proceeds across actors and
#' computes wedges using the provided `wedgeFun` (which characterizes the
#' flavor).
#' @param bigraph An affiliation network; see `is.an`.
#' @param type Character; the type of clustering coefficient (defaults to
#' "global").
#' @param wedgeFun The wedge function (see `wedges`)
#' @param vids A subset of actor node ids at which to evaluate the local
#' clustering coefficient
#' @param add.names Logical; whether to label the matrix rows and columns
#' @export
#' @examples
#' data(women.clique)
#' sapply(c(injequ.wedges, injstr.wedges, indstr.wedges),
#'          transitivity.an, bigraph = women.clique, type = "local")

transitivity.an <-
    function(
        bigraph,
        type = "global",
        wedgeFun = injequ.wedges,
        vids = which(!V(bigraph)$type), add.names = FALSE
    ) {
        if(vcount(bigraph) == 0) {
            if(type == 'global') {
                return(NaN)
            } else if(type == 'local') {
                return(NULL)
            } else return(matrix(NA, nr = 0, nc = 2))
        }
        # Check that nodes are actors
        stopifnot(all(!V(bigraph)$type[vids]))
        # If global or both, need to look at all vertices
        Qs <- if(type == 'global') which(!V(bigraph)$type) else vids
        # Array of 4-paths centered at each Q in Qs
        wedges <- matrix(unlist(lapply(Qs, function(Q) {
            # Return wedge and closed wedge counts at Q
            wedgeFun(bigraph, Q)
        })), nr = 2)
        if(type == 'global') {
            return(sum(wedges[2, ]) / sum(wedges[1, ]))
        }
        if(type == 'local') return(wedges[2, ] / wedges[1, ])
        wedges <- t(wedges)
        if(add.names) {
            rownames(wedges) <- V(bigraph)$name[vids]
            colnames(wedges) <- c('Wedges', 'Closed')
        }
        wedges
    }
