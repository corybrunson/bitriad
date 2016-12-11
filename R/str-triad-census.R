#' Structural triad census
#'
#' This function computes the structural triad census for an affiliation
#' network, a 4-by-2 array of frequency counts for each isomorphism class of
#' triad, modulo an equivalence relation of event nodes by which actors they
#' are tied to, which sum to choose(n, 3), where n is the number of actor nodes.
#' 
#' @name str_triad_census
#' @param bigraph The affiliation network
#' @param verbose Logical; whether to display progress bars
#' @export
str_triad_census <-
  function(bigraph, verbose = FALSE) {
    # Initialize the matrix and define the number of actors
    C <- matrix(0, nrow = 4, ncol = 2)
    n <- length(which(!V(bigraph)$type))
    # Trivial casess (not enough actors)
    if(n < 3) return(C)
    # Trivial case (no events)
    if((vcount(bigraph) - n) == 0) {
      C[1, 1] <- C[1, 1] + choose(n, 3)
      return(C)
    }
    
    # Create one-mode projection
    graph <- actor_projection(bigraph, name = 'id')
    # Leverage one-mode triad census
    C[1:3, 1] <- simple_triad_census(graph)[1:3]
    if(sum(C) == choose(n, 3)) return(C)
    
    # Find all triangles in the projection
    t <- do.call(cbind, cliques(graph, 3, 3))
    # Vector of triad weights
    w <- sapply(1:ncol(t), function(j) {
      shareWeight(bigraph, V(graph)$name[c(t[1, j], t[2, j], t[3, j])])
    })
    w0 <- which(w == 0)
    C[4, 1] <- length(w0)
    
    # Restrict to triads with 3-actor events
    t <- t[, -w0]
    w <- w[-w0]
    # Compute the number of actor pairs with exclusive events in each
    l <- sapply(1:ncol(t), function(j) sum(c(
      edgeWeight(graph, c(t[1, j], t[2, j])),
      edgeWeight(graph, c(t[2, j], t[3, j])),
      edgeWeight(graph, c(t[1, j], t[3, j]))) > w[j]))
    C[, 2] <- tabulate(l + 1, nbins = 4)
    
    # Return the matrix
    stopifnot(sum(C) == choose(n, 3))
    C
  }

#' @rdname str_triad_census
#' @export
structural.triad.census <- str_triad_census
