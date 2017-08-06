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
  C <- triad_census(as.directed(graph))
  if (sum(C) == choose(vcount(graph), 3) & all(C >= 0) & !is.nan(C[1])) {
    C <- C[c(1, 3, 11, 16)]
    if (add.names) names(C) <- 0:3
    return(C)
  }
  # Initialize census and graph size
  n <- vcount(graph)
  # Across edges, tally nodes adjacent to 0, 1, or 2 of the endpoints
  edge_plus <- apply(as_edgelist(graph), 1, function(pair) {
    nbhd <- neighborhood(graph, 1, pair)
    cons <- length(unique(unlist(nbhd))) - 2
    tris <- length(do.call(intersect, nbhd)) - 2
    c('1' = n - cons - 2, '2' = cons - tris, '3' = tris)
  })
  # Store C as row sums, correct for repeats, fill in empty triad count
  C <- c("0" = 0, rowSums(edge_plus) / 1:3)
  C[1] <- choose(n, 3) - sum(C)
  if (!add.names) C <- unname(C)
  C
}

#' @rdname simple_triad_census
#' @export
simple.triad.census <- simple_triad_census
