#' Simple triad census
#' 
#' This function inputs a simple graph and returns a vector of frequency counts
#' for the four isomorphism classes of triad, which therefore sum to
#' choose(n, 3), where n is the number of nodes. This function is needed because
#' igraph::triad.census, from which in small cases a simple triad census can be
#' recovered through strategic coordinate sums, returns nonsense frequencies for
#' many large graphs.
#' @param graph A one-mode network
#' @param rcnames Logical; whether to label the vector coordinates (0 thru 3)
#' @export

simple.triad.census <-
    function(graph) {
        # Use implemented triad census if it makes sense
        C <- triad.census(as.directed(graph))
        if(sum(C) == choose(vcount(graph), 3) & all(C >= 0) & !is.nan(C[1])) {
            C <- C[c(1, 3, 11, 16)]
            return(C)
        }
        # Initialize census and graph size
        n <- vcount(graph)
        # Across edges, tally nodes adjacent to 0, 1, or 2 of the endpoints
        edge.plus <- apply(get.edgelist(graph), 1, function(pair) {
            nbhd <- neighborhood(graph, 1, pair)
            cons <- length(unique(unlist(nbhd))) - 2
            tris <- length(do.call(intersect, nbhd)) - 2
            c('1' = n - cons - 2, '2' = cons - tris, '3' = tris)
        })
        # Store C as row sums, correct for repeats, fill in empty triad count
        C <- c('0' = 0, rowSums(edge.plus) / 1:3)
        C[1] <- choose(n, 3) - sum(C)
        return(unname(C))
    }
