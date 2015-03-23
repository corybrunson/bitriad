#' Triad tallies
#'
#' These functions are called by the full triad census to handle triads of
#' different types using the projection onto actor nodes. The name of each
#' function indicates the number of edges that appear among the three actors of
#' the triad in the projection. (Zero-edge triads do not need to be tallied;
#' the total number of triads is easily calculated, and the difference between
#' this number and the total number of triads with edges gives the number of
#' triads without.)
#' @param graph A one-mode network
#' @param bigraph The ambient affiliation network from which graph is projected

twoTiedTriads <-
    function(graph) {
        # List of open wedges (shortest paths of length 2) up to reversal
        p2 <- do.call(cbind, lapply(
            V(graph)[1:(vcount(graph) - 1)], function(v) {
                d2 <- as.numeric(V(graph)[
                    which(shortest.paths(graph, v, (v + 1):vcount(graph),
                                         weights = NA) == 2) + v
                    ])
                gasp <- get.all.shortest.paths(graph, v, d2, weights = NA)[[1]]
                do.call(cbind, gasp[sapply(gasp, length) == 3])
            }))
        # Horizontal array of sorted edge weight pairs
        if(is.null(p2)) return(NULL) else  wedges <- sapply(
            1:dim(p2)[2],
            function(j) sort(c(edgeWeight(graph, c(p2[1, j], p2[2, j])),
                               edgeWeight(graph, c(p2[2, j], p2[3, j]))),
                             decreasing = TRUE))
        # Make wedges into a data frame
        wedges <- data.frame(x = wedges[1, ], y = wedges[2, ], n = 1)
        # Return the aggregated data frame
        return(aggregate(n ~ x * y, FUN = sum,
                         data = cbind(wedges, n = rep(1, n = dim(wedges)[1]))))
    }
