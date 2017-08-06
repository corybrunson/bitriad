#' @title Simple triad census
#'   
#' @description Given a simple graph, return a vector indicating the number of 
#'   triads of each isomorphism class, namely whether the triad contains zero, 
#'   one, two, or three links. The values must sum to \eqn{\frac{n!}{3!(n-3)!}},
#'   where \eqn{n} is the number of nodes.
#'   
#' @name simple_triad_census
#' @param graph An \code{igraph} object.
#' @param add.names Logical; whether to label the matrix rows and columns.
#' @export
simple_triad_census <- function(graph, add.names = FALSE) {
  # Use implemented triad census if it makes sense
  tc <- triad_census(as.directed(graph))
  if (sum(tc) == choose(vcount(graph), 3) & all(tc >= 0) & !is.nan(tc[1])) {
    tc <- tc[c(1, 3, 11, 16)]
    if (add.names) names(tc) <- 0:3
    return(tc)
  }
  # Initialize census and graph size
  n <- vcount(graph)
  # Across edges, tally nodes adjacent to 0, 1, or 2 of the endpoints
  edge_plus <- apply(as_edgelist(graph), 1, function(pair) {
    nbhd <- neighborhood(graph, 1, pair)
    cons <- length(unique(unlist(nbhd))) - 2
    tris <- length(do.call(intersect, nbhd)) - 2
    c(n - cons - 2, cons - tris, tris)
  })
  # Store 'tc' as row sums, correct for repeats, fill in empty triad count
  tc <- c(0, rowSums(edge_plus) / 1:3)
  tc[1] <- choose(n, 3) - sum(tc)
  if (add.names) names(tc) <- 0:3
  tc
}

#' @rdname simple_triad_census
#' @export
simple.triad.census <- simple_triad_census
