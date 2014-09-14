#' Two-mode triads
#' 
#' @param lambda The vector of pairwise weights
#' @param w The triadwise weight
#' @param actors Actor names (defaults to 'P', 'Q', and 'R')
#' @param events Event names (defaults to positive integers)
#' @export

twomode.triad <-
function(lambda, w, actors = LETTERS[16:18],
         events = if(sum(c(lambda, w)) == 0) 0 else
             as.character(1:sum(c(lambda, w)))) {
    
    # Template triad
    tr <- graph.empty(n = 3, directed = FALSE)
    next.vid <- 4
    
    # Add each type of event node by count
    for(i in 1:3) if(lambda[i] > 0) {
        tr <- add.vertices(tr, nv = lambda[i])
        new.vids <- next.vid:(next.vid + lambda[i] - 1)
        tr <- add.edges(tr, edges = as.vector(cbind(
            rbind(i, new.vids), rbind((i %% 3) + 1, new.vids))))
        next.vid <- next.vid + lambda[i]
    }
    if(w > 0) {
        tr <- add.vertices(tr, nv = w)
        new.vids <- next.vid:(next.vid + w - 1)
        tr <- add.edges(tr, edges = as.vector(cbind(
            rbind(1, new.vids), rbind(2, new.vids), rbind(3, new.vids))))
        next.vid <- next.vid + w
    }
    
    # Give bipartite structure
    V(tr)$name <- c(actors, events)
    V(tr)$type <- c(rep(FALSE, 3), rep(TRUE, sum(c(lambda, w))))
    return(tr)
}
