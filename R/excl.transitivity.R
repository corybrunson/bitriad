#' Affiliation network clustering coefficients
#'
#' Each clustering coefficient can be defined as the proportion of "wedges" that
#' are "closed", for suitable definitions of both terms. The main function,
#' transitivity.an, calls one of the wedge functions and computes the
#' global or local clustering coefficient of the given affiliation network,
#' and if the local, then at the given nodes.
#' @param bigraph An affiliation network.
#' @param type The type of clustering coefficient (defaults to 'global')
#' @param vids A subset of actor node ids at which to evaluate the local
#' clustering coefficient.
#' @export
#' @examples
#' data(women.group)
#' excl.transitivity(women.group)
#' cbind(
#'     project.transitivity(women.group, type = "local"),
#'     opsahl.transitivity(women.group, type = "local"),
#'     excl.transitivity(women.group, type = "local")
#' )

excl.transitivity <-
    function(
        bigraph, type = 'global',
        vids = which(!V(bigraph)$type)
    ) {
        transitivity.an(
            bigraph = bigraph, type = type,
            wedgeFun = indstr.wedges, vids = vids)
    }
