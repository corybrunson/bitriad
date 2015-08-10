#' Classes for all triads centered at a given set of nodes
#' 
#' @param bigraph An affiliation network; see \code{is.an}.
#' @param vids A subset of actor node ids.
#' @return A 5-column matrix of centered triads, each described by its center and exclusive and inclusive event counts.
#' @export

centeredTriads <- function(bigraph, vids) {
    
    # Require consistent indexing (for interchangeability of actor ids below)
    if(!is.an(bigraph)) stop("'bigraph' must be an affiliation network")
    # Actor projection
    graph <- actor.projection(bigraph)
    
    # Neighborhoods (with starting node removed)
    n <- lapply(neighborhood(graph, order = 1, nodes = vids), function(x) x[-1])
    
    # Across all nodes in vids...
    do.call(rbind, lapply(1:length(vids), function(i) {
        
        v <- vids[i]
        
        # Pairs of v's neighbors
        # Note: using nodes from bigraph requires consistent indexing
        # (guaranteed by is.an)
        #n <- setdiff(neighborhood(graph, order = 1, nodes = v)[[1]], v)
        ns <- combn(n[[i]], m = 2)
        
        # Across all pairs of v's neighbors...
        dat <- cbind(v = v, do.call(rbind, lapply(1:ncol(ns), function(j) {
            
            # Triad class, unsorted, with v at the center
            triad.class(bigraph,
                        c(ns[1, j], v, ns[2, j]),
                        sorted = FALSE)
        })))
        #colnames(dat)[3:5] <- c("x", "y", "z")
        dat
    }))
}
