#' Affiliation network clustering coefficients: triads method
#' 
#' This method first classifies every triad centered at each node. The appropriate formula then counts the wedges and closed wedges at each. The method is slower for a single flavor but can be used to produce multiple flavors with negligible additional computational cost.
#' 
#' @param bigraph An affiliation network; see \code{is.an}.
#' @param vids A subset of actor node ids at which to evaluate the local
#' clustering coefficient.
#' @param flavor The flavor of transitivity.
#' @return A two-column matrix of wedge counts and closed wedge counts at the nodes in vids.

transitivity.an.triads <-
    function(
        bigraph,
        vids = which(!V(bigraph)$type),
        flavor
    ) {
        # Data frame of quadruples (w,x,y,z) of triads centered at Qs
        triads <- centeredTriads(bigraph = bigraph, vids = Qs)
        
        # Wedge and closed wedge counts for each triad
        if(length(flavor) == 1) {
            wedges <- triadWedges(triads, flavor = flavor)
        } else {
            wedges <- lapply(flavor, triadWedges, triads = triads)
        }
        wedges
    }
