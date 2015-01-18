#' Affiliation network clustering coefficients
#' 
#' Each clustering coefficient can be defined as the proportion of "wedges" that
#' are "closed", for suitable definitions of both terms. The main function,
#' an.transitivity, calls one of the wedge functions and computes the
#' global or local clustering coefficient of the given affiliation network,
#' and if the local, then at the given nodes. (`watts.strogatz.transitivity`
#' cheats by using the native `transitivity` but produces output consistent
#' with the other variants of `an.transitivity`.)
#' @param bigraph An affiliation network.
#' @param node.type The actor node type in the bigraph object (defaults to 0)
#' @param type The type of clustering coefficient (defaults to 'global')
#' @param stat Whether to compute a `clustering coefficient` or a `transitivity
#' ratio`; defaults to 'coeff'
#' @param vids A subset of actor node ids at which to evaluate the local
#' clustering coefficient.
#' @export

watts.strogatz.transitivity <-
    function(
        bigraph, node.type = 0, type = 'global', stat = 'coeff',
        vids = which(V(bigraph)$type == node.type)
    ) {
        graph <- actor.projection(bigraph, type = node.type)
        if(type == 'global') {
            C <- transitivity(graph, type = 'global')
            if(substr(stat, 1, 5) %in% c('clust', 'coeff')) return(C)
            if(substr(stat, 1, 5) %in% c('trans', 'ratio'))
                return(C / (3 - 2 * C))
        }
        C <- transitivity(graph, type = 'local')
        if(type == 'local') return(C)
        W <- choose(degree(graph), 2)
        data.frame(V = W, T = W * C)
    }
