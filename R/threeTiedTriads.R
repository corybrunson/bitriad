#' Triad tallies
#'
#' These functions are called by the affiliation network triad census to handle
#' triads of different types using the projection onto actors. The name of each
#' function indicates the number of edges that appear among the three actors of
#' the triad in the projection. (Zero-edge triads do not need to be tallied;
#' the total number of triads is easily calculated, and the difference between
#' this number and the total number of triads with edges gives the number of
#' triads without.)
#' @param graph A one-mode network
#' @param bigraph The ambient affiliation network from which graph is projected

threeTiedTriads <-
function(
        bigraph,
        # Construct the one-mode projection if it's not already prepared
        graph = actor.projection(bigraph, name = 'id')
    ) {
        # Triangles are 3-cliques in the one-mode projection
        t <- do.call(cbind, cliques(graph, min = 3, max = 3))
        # If there are no triangles then return an empty list
        if(is.null(t)) return(NULL)
        # Vector of triad weights
        w <- sapply(1:dim(t)[2], function(j) {
            shareWeight(bigraph, V(graph)$name[c(t[1, j], t[2, j], t[3, j])])
        })
        # Horizontal array of sorted triples of edge weights
        ew <- sapply(1:dim(t)[2], function(j) {
            sort(c(edgeWeight(graph, c(t[1, j], t[2, j])),
                   edgeWeight(graph, c(t[2, j], t[3, j])),
                   edgeWeight(graph, c(t[1, j], t[3, j]))),
                 decreasing = TRUE)
        })
        tris <- data.frame(x = ew[1, ] - w, y = ew[2, ] - w,
                           z = ew[3, ] - w, w = w)
        return(aggregate(n ~ x * y * z * w, data = cbind(tris, n = 1),
                         FUN = sum))
    }
