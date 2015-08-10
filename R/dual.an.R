#' Take the dual of an affiliation network
#' 
#' This function negates the logical values of the vertex \code{type} attribute
#' and reorders the vertex ids accordingly, effectively obtaining the dual 
#' affiliation network.
#' @param bigraph An affiliation network; see \code{\link{is.an}}.
#' @examples
#' data(southafrica1905)
#' tab <- table(V(southafrica1905)$type)
#' proj <- actor.projection(dual.an(southafrica1905))
#' vcount(proj) == tab[2]
#' @export

dual.an <-
function(bigraph) {
    if(!is.an(bigraph)) stop('Not an affiliation network')
    V(bigraph)$type <- !V(bigraph)$type
    bigraph
}
