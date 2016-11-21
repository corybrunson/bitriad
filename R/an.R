#' Affiliation network
#' 
#' An affiliation network is, essentially, a bipartite graph. The igraph 
#' function \code{\link{is_bipartite}} tests an igraph object for the logical
#' vertex attribute \code{type} (if it has any vertices at all), which is
#' intended to provide a bipartition of the vertices. It does not, however, test
#' whether edges exist within either part of the partition. The function
#' \code{\link{is_an}} tests this condition. To simplify some functions,
#' affiliation networks, as defined here, must also have their vertices indexed
#' in order of part; all actor vertices must precede all event vertices. The
#' function \code{is_an} tests also for this condition. The coersive function
#' \code{as_an} imposes, by a minimal permutation of vertex labels, provided the
#' \code{igraph} object is bipartite to begin with.
#' 
#' @name an
#' @param graph An igraph object
#' @export
is_an <-
  function(graph) {
    # Must be a graph object
    if(!is_igraph(graph)) return(FALSE)
    if(vcount(graph) == 0) return(TRUE)
    # Must have node types (i.e. be "bipartite")
    if(!('type' %in% vertex_attr_names(graph))) return(FALSE)
    # Vertices must be in order of type
    if(!all(V(graph)$type == sort(V(graph)$type))) return(FALSE)
    # There must be no edges between nodes of the same type
    el <- as_edgelist(graph, names = FALSE)
    all(V(graph)$type[el[, 1]] + V(graph)$type[el[, 2]])
  }

#' @rdname an
#' @export
as_an <-
  function(graph) {
    # Must be a graph object
    if(!is_igraph(graph)) stop('Not an igraph object')
    # Trivial graphs are OK
    if(vcount(graph) == 0) return(graph)
    # Must have node types (i.e. be "bipartite")
    if(!('type' %in% vertex_attr_names(graph))) {
      
      # REPLACE THIS WITH A SCRIPT TO START AT THE FIRST NODE WITH type 0,
      # THEN ASSIGN TYPES ACCORDING TO SHORTEST PATH LENGTH FROM FIRST NODE
      stop('Needs type attribute')
    }
    # There must be no edges between nodes of the same type
    el <- as_edgelist(graph, names = FALSE)
    if(!all(V(graph)$type[el[, 1]] + V(graph)$type[el[, 2]]))
      stop('Type attribute does not form a bipartition')
    # Put nodes in order of type
    ord <- order(order(V(graph)$type))
    permute(graph, ord)
  }
