# Plot an aesthetic triad in a suitable window
plot.triad <- function(lambda, w, S = .001, layout,
                       scale = 0.3, angdir = -1, rot = -pi/2,
                       rot.lambda = c(0, 0, 0), rot.w = pi/12, ...) {
    tr <- twomode.triad(lambda, w)
    q <- sum(c(lambda, w))
    # Default layout
    if(missing(layout)) layout <- wobble.coords(layout.triad(
        lambda = lambda, w = w, scale = scale, angdir = angdir,
        rot = rot, rot.lambda = rot.lambda, rot.w = rot.w), S = S)
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
