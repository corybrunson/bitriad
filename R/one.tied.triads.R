#' Triad tallies
#' 
#' These functions are called by the two-mode triad census to handle triads of
#' different types using the projection onto actor nodes. The name of each
#' function indicates the number of edges that appear among the three actors of
#' the triad in the projection. (Zero-edge triads do not need to be tallied;
#' the total number of triads is easily calculated, and the difference between
#' this number and the total number of triads with edges gives the number of
#' triads without.)
#' @param graph A one-mode network
#' @param bigraph The ambient affiliation network from which graph is projected
#' @param type The actor node type in bigraph
#' @export

one.tied.triads <-
function(graph) {
        # Create a data frame of weights and number of nonadjacent nodes
        counts <- data.frame(
            x = E(graph)$weight,
            n = vcount(graph) - sapply(1:ecount(graph), function(i) {
                length(unique(unlist(neighborhood(graph, 1,
                                                  get.edge(graph, i)))))
            })
        )
        # Return the aggregated data frame
        return(aggregate(n ~ x, data = counts, FUN = sum))
    }
