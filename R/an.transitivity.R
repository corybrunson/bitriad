#' Affiliation network clustering coefficients
#' 
#' Each clustering coefficient can be defined as the proportion of "wedges" that
#' are "closed", for suitable definitions of both terms. The main function,
#' an.transitivity, calls one of the wedge functions and computes the
#' global or local clustering coefficient of the given affiliation network,
#' and if the local, then at the given nodes.
#' @param bigraph An affiliation network.
#' @param node.type The actor node type in the bigraph object (defaults to 0)
#' @param type The type of clustering coefficient (defaults to 'global')
#' @param wedges.fn The wedge function (see the entry on "Wedges")
#' @param vids A subset of actor node ids at which to evaluate the local
#' clustering coefficient
#' @export

an.transitivity <-
    function(
        bigraph, node.type = 0, type = 'global', wedges.fn = injequ.wedges,
        vids = which(V(bigraph)$type == node.type)
    ) {
        # Check that nodes are of the desired type
        stopifnot(all(V(bigraph)$type[vids] == node.type))
        # If global or both, need to look at all vertices
        Qs <- if(type != 'local') which(V(bigraph)$type == node.type) else vids
        # Array of 4-paths centered at each Q in Qs
        wedges <- matrix(unlist(lapply(Qs, function(Q) {
            # Return wedge and closed wedge counts at Q
            return(wedges.fn(bigraph, Q))
        })), nr = 2)
        if(type == 'global') return(sum(wedges[2, ]) / sum(wedges[1, ]))
        if(type == 'local') return(wedges[2, ] / wedges[1, ])
        return(data.frame(V = wedges[1, ], T = wedges[2, ]))
    }
