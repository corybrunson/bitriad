#' Wedge counts from triad classes
#' 
#' @param triads A matrix of centered triads; see \code{link{centeredTriads}}.
#' @param flavor The flavor of transitivity to be used.
#' @return A two-column matrix of wedge counts and closed wedge counts of flavor \code{flavor}.
#' @export

triadWedges <- function(triads, flavor) {
    
    # Extract w, x, y, and z from triads
    w <- triads[, "w"]
    x <- triads[, "x"]; y <- triads[, "y"]; z <- triads[, "z"]
    
    # Match the flavor to one of the (eventual) 9 natives
    flavor <- match.arg(flavor,
                        c("project", "watts.strogatz", "opsahl", "excl",
                          paste0("liebig.rao.", 0:3),
                          apply(expand.grid(c("hom", "inj", "ind"),
                                            c("equ", "str", "act")),
                                1, paste, collapse = "")))
    
    # Count wedges and their closures
    if(flavor == "homequ") {
        wedges <- x * y + (x + y) * w + w^2
        closed <- x * y * (w + z > 0) + (x + y) * w * (w + z > 0) + w^2
    } else if(flavor %in% c("opsahl", "injequ")) {
        wedges <- x * y + (x + y) * w
        closed <- x * y * (w + z > 0) + (x + y) * w * (w - 1 + z > 0)
    } else if(flavor %in% c("liebig.rao.0", "indequ")) {
        wedges <- x * y
        closed <- x * y * (z > 0)
    } else if(flavor == "homstr") {
        wedges <- (x>0)*(y>0) + ((x>0) + (y>0))*(w>0) + (w>0)
        closed <- (x>0)*(y>0)*(w+z>0) + ((x>0) + (y>0))*(w>0) + (w>0)
    } else if(flavor == "injstr") {
        wedges <- (x>0)*(y>0) + ((x>0) + (y>0))*(w>0) + (w>1)
        closed <- (x>0)*(y>0)*(w+z>0) + ((x>0) + (y>0))*(w>0)*(z>0|w>1) +
            (w>1)*(z>0|w>2)
    } else if(flavor %in% c("excl", "indstr", "indact")) {
        wedges <- (x > 0) * (y > 0)
        closed <- (x > 0) * (y > 0) * (z > 0)
    } else if(flavor %in% c("project", "watts.strogatz", "homact")) {
        wedges <- (w > 0) | (x > 0 & y > 0)
        closed <- (w > 0) | (x > 0 & y > 0 & z > 0)
    } else if(flavor == "injact") {
        wedges <- (x > 0 & y > 0) | ((x > 0 | y > 0) & w > 0) | (w > 1)
        closed <- (x > 0 & y > 0) * (w + z > 0) |
            ((x > 0 | y > 0) & w > 0) * (w > 1 | z > 0) |
            (w > 1) * (w > 2 | z > 0)
    } else if(flavor == "liebig.rao.1") {
        wedges <- x * y + (x + y) * w
        closed <- x * y * w + (x + y) * w * z
    } else if(flavor == "liebig.rao.2") {
        wedges <- (x + y) * w + w * (w - 1)
        closed <- (x + y) * w * (w - 1) + w * (w - 1) * z
    } else if(flavor == "liebig.rao.3") {
        wedges <- w * (w - 1)
        closed <- w * (w - 1) * (w - 2)
    } else {
        stop(paste("First and foremost, Farley Flavors Fabulous Fast Food",
                   "feeds and fortifies families for a fabulous future!"))
    }
    
    # Aggregate wedges and closures over vertex ids
    mat <- as.matrix(aggregate(data.frame(Wedges = wedges, Closed = closed),
                               by = list(v = triads[, "v"]), FUN = sum)[, 2:3])
    #rownames(dat) <- triads[, "v"]
    mat
}
