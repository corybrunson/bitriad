#' Dynamic triadic closure
#'
#' Given an affiliation network with time-stamped events, or a simple network
#' with time-stamped edges, compute the proportion of centered triples at which
#' an open wedge exists at some time that is closed at a later time.
#' @param graph An affiliation network with time-stamped events or a simple
#' network with time-stamped edges
#' @param memory Numeric; a duration of time after which events or edges are
#' forgotten. Defaults to Inf
#' @export
#' @examples
#' data(ddggs.group)
#' dyn.transitivity(ddggs.group)
#' cbind(
#'     transitivity(actor.projection(ddggs.group), type = 'local'),
#'     opsahl.transitivity(ddggs.group, type = 'local'),
#'     excl.transitivity(ddggs.group, type = 'local'),
#'     dyn.transitivity(ddggs.group, type = 'local')
#' )

dyn.transitivity <-
    function(graph, memory = Inf, type = 'global', count.closures = FALSE) {
        if(!is.igraph(graph)) {
            stop('Not a graph object')
        }
        if(!is.dyn(graph)) {
            stop('Not a dynamic graph')
        }
        if(is.bipartite(graph)) {
            dyn.transitivity.bigraph(graph,
                                        memory = memory,
                                        type = type,
                                        count.closures = count.closures)
        } else {
            dyn.transitivity.graph(graph,
                                      memory = memory,
                                      type = type,
                                      count.closures = count.closures)
        }
    }
