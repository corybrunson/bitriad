#' @title Affiliation network triads
#'   
#' @description These functions create and operate on triads in affiliation 
#'   networks. In this context, a *triad* is the \code{\link{schedule}} of a 
#'   subset of three distinct actors.
#'   
#' @name triad
#' @param lambda A non-negative integer vector of length three indicating the 
#'   number of events attended by each pair of actors and not by the third 
#'   (*exclusive* events).
#' @param w A non-negative integer indicating the number of events attended by 
#'   all three actors (*inclusive* events).
#' @param actor_names,event_names Actor and event names (actor names default to 
#'   "p", "q", and "r"; event names default to positive integers).
#' @param graph An affiliation network, in some cases must be a triad.
#' @param actors A vector of three actor nodes in \code{graph}.
#' @param as.partition Whether to sort the exclusive events, versus reporting 
#'   them in order of the nodes; defaults to \code{TRUE}.
#' @param format Character matched to "list" or "vector"; whether to return the 
#'   triad class as a list of \eqn{\lambda=(x,y,z)} and \eqn{w} or as a vector
#'   of \eqn{w}, \eqn{x=\lambda_1}, \eqn{y=\lambda_2}, and \eqn{z=\lambda_3}.
#' @param triad An affiliation network with exactly three distinct actors.
#' @param scale A scaling parameter for the entire plot.
#' @param angdir A rotation direction parameter (\code{-1} for clockwise, 
#'   \code{1} for counter-clockwise).
#' @param rot,rot_lambda,rot_w Angular orientation parameters for the entire 
#'   triad, for the exclusive events of two actors, and for the inclusive events
#'   of all three actors.
#' @param layout A two-column numeric matrix interpretable as a 
#'   \code{\link[igraph]{layout}}.
#' @param prettify Logical; whether to use \code{prettify_an} to adjust the 
#'   aesthetics of a triad before plotting it.
#' @param cex Node size scaling parameter.
#' @param xlim,ylim Custom bounds on the horizontal and vertical axes.
#' @param ... Additional arguments passed to \code{\link[igraph]{plot.igraph}}.
NULL

#' @rdname triad
#' @export
make_triad <- function(
  lambda, w,
  actor_names = c("p", "q", "r"),
  event_names = if(sum(c(lambda, w)) == 0) c() else
    as.character(1:sum(c(lambda, w)))
) {
  
  # make edgelist
  el <- do.call(rbind, list(
    # events attended by p and q
    cbind(rep(c(1, 2), each = lambda[1]),
          rep(3 + seq_len(lambda[1]), times = 2)),
    # events attended by q and r
    cbind(rep(c(2, 3), each = lambda[2]),
          rep(3 + lambda[1] + seq_len(lambda[2]), times = 2)),
    # events attended by p and r
    cbind(rep(c(1, 3), each = lambda[3]),
          rep(3 + sum(lambda[1:2]) + seq_len(lambda[3]), times = 2)),
    # events attended by p, q, and r
    cbind(rep(1:3, each = w),
          rep(3 + sum(lambda) + seq_len(w), times = 3))
  ))
  
  # make affiliation network
  tr <- graph_from_edgelist(el = el, directed = FALSE)
  V(tr)$name <- c(actor_names, event_names)
  V(tr)$type <- c(rep(FALSE, 3), rep(TRUE, sum(c(lambda, w))))
  as_an(tr)
}

#' @rdname triad
#' @export
is_triad <- function(graph) {
  if (!is_an(graph)) {
    warning("Object 'graph' is not an affiliation network.")
    return(FALSE)
  }
  if (actor_count(graph) != 3) return(FALSE)
  if (any(degree(graph, V(graph)$type == TRUE) <= 1)) return(FALSE)
  TRUE
}

#' @rdname triad
#' @export
triad_class <- function(
  graph,
  actors = V(graph)[V(graph)$type == FALSE],
  as.partition = TRUE,
  format = "list"
) {
  stopifnot(is_an(graph))
  stopifnot(all(V(graph)[actors]$type == FALSE))
  stopifnot(length(actors) == 3)
  
  # count inclusive and exclusive events
  actor_events <- neighborhood(graph, order = 1, nodes = actors)
  w <- length(which(table(unlist(actor_events)) == 3))
  lambda <- sapply(1:3, function(i) {
    length(which(table(unlist(actor_events[c(i, i %% 3 + 1)])) > 1))
  }) - w
  # represent 'lambda' as a partition (non-increasing)
  if (as.partition) lambda <- sort(lambda, decreasing = TRUE)
  
  # return class
  format <- match.arg(format, c("list", "vector"))
  if (format == "list") {
    list(lambda = lambda, w = w)
  } else {
    c(w = w, x = lambda[1], y = lambda[2], z = lambda[3])
  }
}

#' @rdname triad
#' @export
layout_triad <- function(
  triad = NULL, lambda = NULL, w = NULL,
  scale = 0.3,
  angdir = -1, rot = -pi/2, rot_lambda = c(0, 0, 0), rot_w = pi/12
) {
  if (!is.null(triad)) {
    if (!is.null(lambda) | !is.null(w)) {
      stop("Provide either a triad 'triad' or a class via 'lambda' and 'w'.")
    }
    tr_class <- triad_class(triad)
    lambda <- tr_class$lambda
    w <- tr_class$w
  }
  
  # angles of actors from origin
  theta <- angdir * (0:2 * 2*pi/3 + rot)
  # matrix of coordinates
  mat <- matrix(c(cos(theta), sin(theta)), ncol = 2)
  # NEEDS WORK TO COORDINATE DIFFERENT VALUES OF angdir, rot, and rot.*
  if (lambda[1]) {
    psi <- 0:(lambda[1] - 1) * 2*pi / lambda[1] + 7*pi/6 + rot_lambda[1]
    mat <- rbind(mat, matrix(c(
      (lambda[1] > 1) * scale * cos(psi) + cos(pi/6),
      (lambda[1] > 1) * scale * sin(psi) + sin(pi/6)),
      ncol = 2))
  }
  if (lambda[2]) {
    psi <- 0:(lambda[2] - 1) * 2*pi /
      lambda[2] + pi/2 + rot_lambda[2]
    mat <- rbind(mat, matrix(c(
      (lambda[2] > 1) * scale * cos(psi) + cos(3*pi/2),
      (lambda[2] > 1) * scale * sin(psi) + sin(3*pi/2)),
      ncol = 2))
  }
  if (lambda[3]) {
    psi <- 0:(lambda[3] - 1) * 2*pi / lambda[3] - pi/6 +
      rot_lambda[3]
    mat <- rbind(mat, matrix(c(
      (lambda[3] > 1) * scale * cos(psi) + cos(5*pi/6),
      (lambda[3] > 1) * scale * sin(psi) + sin(5*pi/6)),
      ncol = 2))
  }
  if (w) {
    psi <- 0:(w - 1) * 2*pi / w + rot_w
    mat <- rbind(mat,
                 matrix((w > 1) * scale * c(cos(psi), sin(psi)),
                        ncol = 2))
  }
  mat
}

#' @rdname triad
#' @export
plot_triad <- function(
  triad = NULL, lambda = NULL, w = NULL,
  layout = NULL,
  prettify = TRUE,
  cex = 1,
  scale = 0.3,
  angdir = -1, rot = -pi/2, rot_lambda = c(0, 0, 0), rot_w = pi/12,
  actor_names = c("p", "q", "r"),
  event_names = if(sum(c(lambda, w)) == 0) c() else
    as.character(1:sum(c(lambda, w))),
  xlim = NULL, ylim = NULL,
  ...
) {
  if (!is.null(triad)) {
    if (!is.null(lambda) | !is.null(w)) {
      stop("Provide either a triad 'triad' or a class via 'lambda' and 'w'.")
    }
    tr_class <- triad_class(triad)
    lambda <- tr_class$lambda
    w <- tr_class$w
  } else {
    triad <- make_triad(lambda, w,
                        actor_names = actor_names, event_names = event_names)
  }
  if (prettify) triad <- prettify_an(triad)
  q <- sum(c(lambda, w))
  
  # default layout
  if (is.null(layout)) {
    layout <- layout_triad(
      lambda = lambda, w = w,
      scale = scale,
      angdir = angdir, rot = rot, rot_lambda = rot_lambda, rot_w = rot_w
    )
  }
  if (is.null(xlim)) xlim <- c(-1.4, 1.4)
  if (is.null(ylim)) ylim <- c(-1.4, 1.4)
  
  # plot graph in a slightly widened frame
  plot.igraph(
    triad,
    layout = layout,
    xlim = xlim, ylim = ylim,
    vertex.label = V(triad)$name,
    vertex.size = V(triad)$size * cex,
    rescale = FALSE, asp = 0
  )
  # return the triad invisibly
  invisible(triad)
}

#' @rdname triad
#' @export
an_triad <- function(...) {
  .Deprecated("make_triad")
  make_triad(...)
}

#' @rdname triad
#' @export
is.triad <- is_triad

#' @rdname triad
#' @export
triad.class <- triad_class

#' @rdname triad
#' @export
an.triad <- function(...) {
  .Deprecated("make_triad")
  make_triad(...)
}

#' @rdname triad
#' @export
layout.triad <- layout_triad

#' @rdname triad
#' @export
plotTriad <- plot_triad
