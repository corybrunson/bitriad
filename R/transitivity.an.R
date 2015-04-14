#' Affiliation network clustering coefficients
#' 
#' This function takes an affiliation network and a "flavor" of transitivity
#' (triadic closure) to compute thereon. The calculations are performed locally
#' and can be returned in either of three formats: global (a single statistic),
#' local (a vector of values labeled by actor names), and raw. Each flavor is
#' defined as a proportion of "wedges" that are "closed", for suitable
#' definitions of both terms. The raw format returns a 2-column matrix, each row
#' of which gives the number of wedges and the number of closed wedges centered
#' at an actor. The function `transitivity.an` is a shell that proceeds across
#' actors and computes wedges using the provided `wedge.fun`.
#' @param bigraph An affiliation network.
#' @param type Character; the type of clustering coefficient (defaults to
#' "global").
#' @param stat Character; the form of the statistic (matched to "clustering" or
#' "transitivity"; defaults to "clust").
#' @param wedge.fun The wedge function (see the entry on `wedges`)
#' @param vids A subset of actor node ids at which to evaluate the local
#' clustering coefficient
#' @param add.names Logical; whether to label the matrix rows and columns
#' @export
#' @examples
#' data(davis.clique)
#' sapply(c(injequ.wedges, injstr.wedges, indstr.wedges),
#'          transitivity.an, bigraph = davis.clique, type = "local")

transitivity.an <-
    function(
        bigraph,
        type = "global", stat = "clust",
        wedge.fun = injequ.wedges,
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
            wedge.fun(bigraph, Q)
        })), nr = 2)
        if(type == 'global') {
            C <- sum(wedges[2, ]) / sum(wedges[1, ])
            stat <- match.arg(stat, c("clustering", "transitivity"))
            if(stat == "clustering") {
                return(C)
            } else {
                return(C / (3 - 2 * C))
            }
        }
        if(type == 'local') return(wedges[2, ] / wedges[1, ])
        wedges <- t(wedges)
        if(add.names) {
            rownames(wedges) <- V(bigraph)$name[vids]
            colnames(wedges) <- c('Wedges', 'Closed')
        }
        wedges
    }
