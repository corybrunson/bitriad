#' Two-mode triads
#' 
#' @param l Layout matrix, having two columns
#' @param S Wobble coefficient (should be small, defaults to .001)
#' @export

# Wobble layout coordinates a bit
wobble.coords <-
function(l, S = .001) {
    require(MASS)
    l + t(sapply(1:nrow(l),
                 function(x) mvrnorm(n = ncol(l), mu = 0, Sigma = S)))
}
