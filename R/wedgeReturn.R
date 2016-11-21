#' Compress a wedgelist into a desired statistic
#' 
#' @param wedges A two-column matrix of wedge and closed wedge counts centered
#'   at each of some collection of nodes.
#' @param type Character; the type of clustering coefficient (defaults to 
#'   "global").
#' @param add.names Logical; whether to label the matrix rows and columns
#' @return A transitivity statistic of type \code{type} calculated from the
#'   wedges \code{wedges}, in a format that can be fed into \code{sapply}.
wedgeReturn <-
  function(wedges, type, add.names) {
    
    # Global
    if(type == "global") {
      return(sum(wedges[, 2]) / sum(wedges[, 1]))
    }
    
    # Local
    if(type == "local") return(as.vector(wedges[, 2] / wedges[, 1]))
    
    # Otherwise
    #if(add.names) {
    #  rownames(wedges) <- V(bigraph)$name[vids]
    #  colnames(wedges) <- c("Wedges", "Closed")
    #}
    wedges
  }
