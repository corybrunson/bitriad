#' Two-mode triads
#' 
#' @param lambda The vector of pairwise weights
#' @param w The triadwise weight
#' @export

triad.plot <-
function(lambda, w, layout, scale = 0.3, angdir = -1,
         rot = -pi/2, rot.lambda = c(0, 0, 0), rot.w = pi/12, ...) {
    tr <- twomode.triad(lambda, w)
    q <- sum(c(lambda, w))
    # Default layout
    if(missing(layout)) layout <- triad.layout(
        lambda = lambda, w = w, scale = scale, angdir = angdir,
        rot = rot, rot.lambda = rot.lambda, rot.w = rot.w)
    # Plot graph in a slightly widened frame
    plot(tr, layout = layout, xlim = c(-1.3, 1.3), ylim = c(-1.3, 1.3),
         vertex.label = V(tr)$name,
         vertex.shape = c(rep(c('circle', 'square'), c(3, q))),
         vertex.size = c(rep(c(34, 28), c(3, q))),
         vertex.color = c(rep(c('SkyBlue2', 'lightcoral'), c(3, q))),
         vertex.label.family = 'sans', vertex.label.font = 2,
         vertex.label.color = 'white',
         edge.width = 2, edge.color = 'black', rescale = FALSE, asp = 0)
}
