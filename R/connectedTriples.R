#' Tally and classify triads in an affiliation network
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

connectedTriples <-
function(
        bigraph,
        # Construct the one-mode projection if it's not already prepared
        graph = actor.projection(bigraph, name = 'id')
    ) {
        trips <- do.call(rbind, lapply(1:vcount(graph), function(i) {
            nbhd <- neighborhood(graph, 1, i)[[1]]
            # Skip nodes with not enough neighbors
            if(length(nbhd) < 2) return(NULL)
            # horizontal array of pairs of neighbors of i
            v <- combn(setdiff(nbhd, i), 2)
            # vector of triad weights
            w <- sapply(1:dim(v)[2], function(j) {
                shareWeight(bigraph, V(graph)$name[c(i, v[, j])])
            })
            # horizontal array of sorted triples of edge weights
            ew <- sapply(1:dim(v)[2], function(j) {
                sort(c(edgeWeight(graph, c(i, v[1, j])),
                       edgeWeight(graph, c(i, v[2, j])),
                       edgeWeight(graph, c(v[1, j], v[2, j]))),
                     decreasing = TRUE)
            })
            # vertical array of pair and triad weights
            return(data.frame(x = ew[1, ] - w, y = ew[2, ] - w,
                              z = ew[3, ] - w, w = w))
        }))
        return(aggregate(n ~ x * y * z * w, FUN = sum,
                         data = cbind(trips,
                                      n = 1 - (trips$z + trips$w > 0) * 2 / 3)))
    }
