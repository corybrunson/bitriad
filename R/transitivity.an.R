#' Affiliation network clustering coefficients
#' 
#' This function computes a given flavor of transitivity (triadic closure) on a
#' given affiliation network. The calculations are performed locally. Each
#' flavor is defined as a proportion of "wedges" that are "closed", for suitable
#' definitions of both terms. The function `transitivity.an` is a shell that
#' proceeds across actors and computes wedges using the provided `wedgeFun`.
#' These functions count the "wedges", and among them the "closed" ones,
#' centered at a given actor node in a given affiliation network.
#' @param bigraph An affiliation network; see \code{is.an}.
#' @param type Character; the type of clustering coefficient (defaults to
#' "global").
#' @param vids A subset of actor node ids at which to evaluate the local
#' clustering coefficient.
#' @param wedgeFun The wedge function; overrides \code{flavor}.
#' @param flavor The flavor of transitivity to be used; overridden by \code{wedgeFun}.
#' @param add.names Logical; whether to label the matrix rows and columns.
#' @return If \code{type} is "global", the global clustering coefficient of the
#' network; if "local", the local clustering coefficients of the actors;
#' otherwise, a 2-column matrix, each row of which gives the number of wedges
#' and the number of closed wedges centered at each actor.
#' @export
#' @examples
#' data(women.clique)
#' sapply(c(injequ.wedges, injstr.wedges, indstr.wedges),
#'          transitivity.an, bigraph = women.clique, type = "local")

transitivity.an <-
    function(
        bigraph,
        type = "global",
        wedgeFun,
        flavor,
        vids = which(!V(bigraph)$type),
        add.names = FALSE
    ) {
        if(vcount(bigraph) == 0) {
            if(type == "global") {
                return(NaN)
            } else if(type == "local") {
                return(NULL)
            } else return(matrix(NA, nr = 0, nc = 2))
        }
        # Check that nodes are actors
        stopifnot(all(!V(bigraph)$type[vids]))
        # If global or both, need to look at all vertices
        Qs <- if(type == "global") which(!V(bigraph)$type) else vids
        
        if(missing(wedgeFun)) {
            if(missing(flavor)) {
                stop("Need a wedge function or a flavor")
            } else {
                wedges <- transitivity.an.triads(bigraph, Qs, flavor)
            }
        } else {
            if(!missing(flavor))
                warning("Wedge function provided; overriding flavor")
            wedges <- transitivity.an.wedges(bigraph, Qs, wedgeFun)
        }
        
        # Return appropriate statistics
        if(mode(wedges) == "list") {
            do.call(cbind, lapply(wedges, wedgeReturn,
                                  type = type, add.names = add.names))
        } else {
            wedgeReturn(wedges, type = type, add.names = add.names)
        }
    }
