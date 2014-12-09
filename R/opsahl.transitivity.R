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
#' clustering coefficient.
#' @export

opsahl.transitivity <-
    function(
        bigraph, node.type = 0, type = 'global',
        vids = which(V(bigraph)$type == node.type)
    ) {
        an.transitivity(
            bigraph = bigraph, node.type = node.type, type = type,
            wedges.fn = injequ.wedges, vids = vids)
    }
