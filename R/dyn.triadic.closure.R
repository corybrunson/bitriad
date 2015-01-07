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

dyn.triadic.closure <-
    function(graph, memory = Inf, type = 'global', count.closures = FALSE) {
        if(!is.igraph(graph)) {
            stop('Not a graph object')
        }
        if(!is.dynamic(graph)) {
            stop('Not a dynamic graph')
        }
        if(is.bipartite(graph)) {
            dyn.triadic.closure.bigraph(graph,
                                        memory = memory,
                                        type = type,
                                        count.closures = count.closures)
        } else {
            dyn.triadic.closure.graph(graph,
                                      memory = memory,
                                      type = type,
                                      count.closures = count.closures)
        }
    }
