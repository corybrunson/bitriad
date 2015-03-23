#' Triad tallies
#' 
#' These functions are called by the affiliation network triad census to handle
#' triads of different types using the projection onto actors The name of each
#' function indicates the number of edges that appear among the three actors of
#' the triad in the projection. (Zero-edge triads do not need to be tallied;
#' the total number of triads is easily calculated, and the difference between
#' this number and the total number of triads with edges gives the number of
#' triads without.)
#' @param graph An igraph object.

oneTiedTriads <-
    function(graph) {
        # Create a data frame of weights and number of nonadjacent nodes
        counts <- data.frame(
            x = E(graph)$weight,
            n = vcount(graph) - as.numeric(sapply(1:ecount(graph), function(i) {
                length(unique(unlist(neighborhood(graph, order = 1,
                                                  get.edge(graph, i)))))
            }))
        )
        # Return the aggregated data frame
        return(aggregate(n ~ x, data = counts, FUN = sum))
    }
