#' Affiliation network triads
#' 
#' @param lambda The vector of pairwise weights
#' @param w The triadwise weight
#' @export

triad.layout <-
    function(lambda, w, scale = 0.3, angdir = -1, rot = -pi/2,
             rot.lambda = c(0, 0, 0), rot.w = pi/12) {
        graph <- twomode.triad(lambda, w)
        # Angles of actors from origin
        theta <- angdir * (0:2 * 2*pi/3 + rot)
        # Matrix of coordinates
        mat <- matrix(c(cos(theta), sin(theta)), nc = 2)
        # NEEDS WORK TO COORDINATE DIFFERENT VALUES OF angdir, rot, and rot.*
        if(lambda[1]) {
            psi <- 0:(lambda[1] - 1) * 2*pi / lambda[1] + 7*pi/6 + rot.lambda[1]
            mat <- rbind(mat, matrix(c(
                (lambda[1] > 1) * scale * cos(psi) + cos(pi/6),
                (lambda[1] > 1) * scale * sin(psi) + sin(pi/6)),
                nc = 2))
        }
        if(lambda[2]) {
            psi <- 0:(lambda[2] - 1) * 2*pi /
                lambda[2] + pi/2 + rot.lambda[2]
            mat <- rbind(mat, matrix(c(
                (lambda[2] > 1) * scale * cos(psi) + cos(3*pi/2),
                (lambda[2] > 1) * scale * sin(psi) + sin(3*pi/2)),
                nc = 2))
        }
        if(lambda[3]) {
            psi <- 0:(lambda[3] - 1) * 2*pi / lambda[3] - pi/6 +
                rot.lambda[3]
            mat <- rbind(mat, matrix(c(
                (lambda[3] > 1) * scale * cos(psi) + cos(5*pi/6),
                (lambda[3] > 1) * scale * sin(psi) + sin(5*pi/6)),
                nc = 2))
        }
        if(w) {
            psi <- 0:(w - 1) * 2*pi / w + rot.w
            mat <- rbind(mat,
                         matrix((w > 1) * scale * c(cos(psi), sin(psi)),
                                nc = 2))
        }
        mat
    }
