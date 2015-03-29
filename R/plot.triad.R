#' Plot affiliation network triads
#' 
#' @param lambda The vector of pairwise weights
#' @param w The triadwise weight
#' @param layout A layout matrix for the vertices of the triad (if missing, a
#' standard layout is constructed)
#' @param scale A scaling parameter
#' @param angdir A rotation direction parameter (-1 for clockwise, 1 for
#' counter-clockwise)
#' @param rot An orientation parameter for the entire triad
#' @param rot.lambda An orientation parameter specifically for exclusive events
#' @param rot.w An orientation parameter specifically for inclusive events
#' @export

plot.triad <-
    function(
        lambda, w, layout, scale = 0.3, cex = 1, angdir = -1,
        rot = -pi/2, rot.lambda = c(0, 0, 0), rot.w = pi/12,
        actors = letters[16:18],
        events = if(sum(c(lambda, w)) == 0) c() else
            as.character(1:sum(c(lambda, w))),
        xlim, ylim, ...) {
        tr <- triad.an(lambda, w, actors = actors, events = events)
        q <- sum(c(lambda, w))
        # Default layout
        if(missing(layout)) layout <- layout.triad(
            lambda = lambda, w = w, scale = scale, angdir = angdir,
            rot = rot, rot.lambda = rot.lambda, rot.w = rot.w)
        if(missing(xlim)) xlim <- c(-1.4, 1.4)
        if(missing(ylim)) ylim <- c(-1.4, 1.4)
        # Plot graph in a slightly widened frame
        plot(tr, layout = layout,
             xlim = xlim, ylim = ylim,
             vertex.label = V(tr)$name,
             vertex.shape = c(rep(c('circle', 'square'), c(3, q))),
             vertex.size = c(rep(c(34, 28), c(3, q))) * cex,
             vertex.color = c(rep(c('SkyBlue2', 'lightcoral'), c(3, q))),
             vertex.label.family = 'sans', vertex.label.font = 2,
             vertex.label.color = 'white',
             edge.width = 2, edge.color = 'black', rescale = FALSE, asp = 0)
    }
