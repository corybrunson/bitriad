#' Triad classification
#' 
#' Given three actor nodes in an affiliation network, determine the isomorphism 
#' class of the triad they form by its index (a partition of three parts and a 
#' nonnegative integer).
#' 
#' @name triad_class
#' @param bigraph An affiliation network
#' @param v A subset of three actor nodes in bigraph
#' @param sorted Whether to sort the exclusive events, versus reporting them in
#'   order of the nodes; defaults to TRUE
#' @export
triad_class <-
  function(bigraph, v, sorted = TRUE) {
    
    # Make sure all three nodes in v are actors
    stopifnot(all(!V(bigraph)$type[v]) & length(v) == 3)
    
    # Identify each actor's events
    ns <- neighborhood(bigraph, order = 1, nodes = v)
    
    # w is the number of events that appear in all three neighborhoods
    w <- length(which(table(unlist(ns)) > 2))
    
    # x, y, z are the event counts in each intersection of two neighborhoods
    lambda <- sapply(1:3, function(i) {
      length(which(table(unlist(ns[c(i, i %% 3 + 1)])) > 1))
    }) - w
    
    # Sort if desired
    if(sorted) lambda <- sort(lambda, decreasing = TRUE)
    
    # Return class
    c(w = w, x = lambda[1], y = lambda[2], z = lambda[3])
  }

#' @rdname triad_class
#' @export
triad.class <- triad_class
