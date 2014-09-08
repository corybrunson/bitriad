# Chcek whether a graph is a triad: Must have 3 actor nodes (type = 0) and
# all event nodes (type = 1) must be tied to at least two actor nodes
is.triad <- function(graph) {
    # Must have node types (i.e. be "bipartite")
    if(is.null(V(graph)$type)) return(FALSE)
    # Must have exactly three actor nodes (type = 0)
    if(length(which(V(graph)$type == 0)) != 3) return(FALSE)
    # Must have no edges among actors or among events
    if(!all(rowSums(matrix(V(tr)$type[get.edgelist(tr, names = FALSE)],
                           nc = 2)) == 1)) return(FALSE)
    # Must have no trivial events
    if(!all(degree(graph)[which(V(graph)$type == 1)] > 1)) return(FALSE)
    # That's it
    return(TRUE)
}
