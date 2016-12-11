#' Affiliation network triads
#' 
#' @name triad
#' @param graph An affiliation network; see \code{\link{is_an}}.
#' @param lambda The vector of pairwise weights
#' @param w The triadwise weight
#' @param actors Actor names (defaults to "P", "Q", and "R")
#' @param events Event names (defaults to positive integers)
#' @param scale A scaling parameter
#' @param angdir A rotation direction parameter (-1 for clockwise, 1 for
#' counter-clockwise)
#' @param rot An orientation parameter for the entire triad
#' @param rot_lambda An orientation parameter specifically for exclusive events
#' @param rot_w An orientation parameter specifically for inclusive events
#' @param layout Two-column matrix interpretable as an igraph layout
#' @param cex Node size scaling parameter
#' @param xlim Bounds on the horizontal axis
#' @param ylim Bounds on the vertical axis
#' @param ... Additional arguments passed to \code{\link{plot}}.
#' @export
is_triad <-
  function(graph) {
    # Must have node types (i.e. be "bipartite")
    if(is.null(V(graph)$type)) return(FALSE)
    # Must have exactly three actor nodes (type = 0)
    if(length(which(!V(graph)$type)) != 3) return(FALSE)
    # Must have no edges among actors or among events
    if(!all(rowSums(matrix(V(graph)$type[as_edgelist(graph, names = FALSE)],
                           ncol = 2)) == 1)) return(FALSE)
    # Must have no trivial events
    if(!all(degree(graph)[which(V(graph)$type)] > 1)) return(FALSE)
    # That's it
    TRUE
  }

#' @rdname triad
#' @export
an_triad <-
  function(lambda, w,
           actors = letters[16:18],
           events = if(sum(c(lambda, w)) == 0) c() else
             as.character(1:sum(c(lambda, w)))) {
    
    # Template triad
    tr <- make_empty_graph(n = 3, directed = FALSE)
    next_vid <- 4
    
    # Add each type of event node by count
    for(i in 1:3) if(lambda[i] > 0) {
      tr <- add_vertices(tr, nv = lambda[i])
      new_vids <- next_vid:(next_vid + lambda[i] - 1)
      tr <- add_edges(tr, edges = as.vector(cbind(
        rbind(i, new_vids), rbind((i %% 3) + 1, new_vids))))
      next_vid <- next_vid + lambda[i]
    }
    if(w > 0) {
      tr <- add_vertices(tr, nv = w)
      new_vids <- next_vid:(next_vid + w - 1)
      tr <- add_edges(tr, edges = as.vector(cbind(
        rbind(1, new_vids), rbind(2, new_vids), rbind(3, new_vids))))
      next_vid <- next_vid + w
    }
    
    # Give bipartite structure
    V(tr)$name <- c(actors, events)
    V(tr)$type <- c(rep(FALSE, 3), rep(TRUE, sum(c(lambda, w))))
    return(tr)
  }

#' @rdname triad
#' @export
layout_triad <-
  function(lambda, w, scale = 0.3, angdir = -1, rot = -pi/2,
           rot_lambda = c(0, 0, 0), rot_w = pi/12) {
    graph <- an_triad(lambda, w)
    # Angles of actors from origin
    theta <- angdir * (0:2 * 2*pi/3 + rot)
    # Matrix of coordinates
    mat <- matrix(c(cos(theta), sin(theta)), ncol = 2)
    # NEEDS WORK TO COORDINATE DIFFERENT VALUES OF angdir, rot, and rot.*
    if(lambda[1]) {
      psi <- 0:(lambda[1] - 1) * 2*pi / lambda[1] + 7*pi/6 + rot_lambda[1]
      mat <- rbind(mat, matrix(c(
        (lambda[1] > 1) * scale * cos(psi) + cos(pi/6),
        (lambda[1] > 1) * scale * sin(psi) + sin(pi/6)),
        ncol = 2))
    }
    if(lambda[2]) {
      psi <- 0:(lambda[2] - 1) * 2*pi /
        lambda[2] + pi/2 + rot_lambda[2]
      mat <- rbind(mat, matrix(c(
        (lambda[2] > 1) * scale * cos(psi) + cos(3*pi/2),
        (lambda[2] > 1) * scale * sin(psi) + sin(3*pi/2)),
        ncol = 2))
    }
    if(lambda[3]) {
      psi <- 0:(lambda[3] - 1) * 2*pi / lambda[3] - pi/6 +
        rot_lambda[3]
      mat <- rbind(mat, matrix(c(
        (lambda[3] > 1) * scale * cos(psi) + cos(5*pi/6),
        (lambda[3] > 1) * scale * sin(psi) + sin(5*pi/6)),
        ncol = 2))
    }
    if(w) {
      psi <- 0:(w - 1) * 2*pi / w + rot_w
      mat <- rbind(mat,
                   matrix((w > 1) * scale * c(cos(psi), sin(psi)),
                          ncol = 2))
    }
    mat
  }

#' @rdname triad
#' @export
plot_triad <-
  function(
    lambda, w, layout, scale = 0.3, cex = 1, angdir = -1,
    rot = -pi/2, rot_lambda = c(0, 0, 0), rot_w = pi/12,
    actors = letters[16:18],
    events = if(sum(c(lambda, w)) == 0) c() else
      as.character(1:sum(c(lambda, w))),
    xlim, ylim, ...) {
    tr <- an_triad(lambda, w, actors = actors, events = events)
    q <- sum(c(lambda, w))
    # Default layout
    if(missing(layout)) layout <- layout_triad(
      lambda = lambda, w = w, scale = scale, angdir = angdir,
      rot = rot, rot_lambda = rot_lambda, rot_w = rot_w)
    if(missing(xlim)) xlim <- c(-1.4, 1.4)
    if(missing(ylim)) ylim <- c(-1.4, 1.4)
    # Plot graph in a slightly widened frame
    graphics::plot(tr, layout = layout,
         xlim = xlim, ylim = ylim,
         vertex.label = V(tr)$name,
         vertex.shape = c(rep(c("circle", "square"), c(3, q))),
         vertex.size = c(rep(c(34, 28), c(3, q))) * cex,
         vertex.color = c(rep(c("SkyBlue2", "lightcoral"), c(3, q))),
         vertex.label.family = "sans", vertex.label.font = 2,
         vertex.label.color = "white",
         edge.width = 2, edge.color = "black", rescale = FALSE, asp = 0)
  }

#' @rdname triad
#' @export
is.triad <- is_triad

#' @rdname triad
#' @export
an.triad <- an_triad

#' @rdname triad
#' @export
layout.triad <- layout_triad

#' @rdname triad
#' @export
plotTriad <- plot_triad
