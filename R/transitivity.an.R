#' Affiliation network clustering coefficients
#'
#' Each clustering coefficient can be defined as the proportion of "wedges" that
#' are "closed", for suitable definitions of both terms. The main function,
#' transitivity.an, calls one of the wedge functions and computes the
#' global or local clustering coefficient of the given affiliation network,
#' and if the local, then at the given nodes.
#' @param bigraph An affiliation network.
#' @param type Character; the type of clustering coefficient (defaults to
#' "global").
#' @param stat Character; the form of the statistic (matched to "clustering" or
#' "transitivity"; defaults to "clust").
#' @param wedges.fn The wedge function (see the entry on `wedges`)
#' @param vids A subset of actor node ids at which to evaluate the local
#' clustering coefficient
#' @param add.names Logical; whether to label the matrix rows and columns
#' @export
#' @examples
#' data(davis.clique)
#' transitivity.table <- sapply(
#'   c(injequ.wedges, injstr.wedges, indstr.wedges),
#'   transitivity.an, bigraph = davis.clique, type = "local"
#' )

transitivity.an <-
    function(
        bigraph,
        type = "global", stat = "clust",
        wedges.fn = injequ.wedges,
        vids = which(!V(bigraph)$type), add.names = FALSE
    ) {
        if(vcount(bigraph) == 0) {
            if(type == 'global') {
                return(NaN)
            } else if(type == 'local') {
                return(NULL)
            } else return(matrix(NA, nr = 0, nc = 2))
        }
        # Check that nodes are of the desired type
        stopifnot(all(!V(bigraph)$type[vids]))
        # If global or both, need to look at all vertices
        Qs <- if(type == 'global') which(!V(bigraph)$type) else vids
        # Array of 4-paths centered at each Q in Qs
        wedges <- matrix(unlist(lapply(Qs, function(Q) {
            # Return wedge and closed wedge counts at Q
            return(wedges.fn(bigraph, Q))
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
        #data.frame(V = wedges[1, ], T = wedges[2, ])
    }
