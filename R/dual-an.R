#' Take the dual of an affiliation network
#' 
#' This function negates the logical values of the vertex \code{type} attribute
#' and reorders the vertex ids accordingly, effectively obtaining the dual 
#' affiliation network.
#' 
#' @name dual
#' @import igraph
#' @param bigraph An affiliation network; see \code{\link{is_an}}.
#' @examples
#' data(women_clique)
#' tab <- table(V(women_clique)$type)
#' proj <- actor_projection(dual_an(women_clique))
#' vcount(proj) == tab[2]
#' @export
dual_an <-
  function(bigraph) {
    if(!is_an(bigraph)) stop('Not an affiliation network')
    V(bigraph)$type <- !V(bigraph)$type
    bigraph
  }

#' @rdname dual
#' @export
dual.an <- dual_an
