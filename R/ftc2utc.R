#' Full triad census to uniformity triad census
#' 
#' TO BE DELETED
#' 
#' @param tc A full two-mode triad census (not necessarily labeled)
ftc2utc <-
  function(tc) {
    # Trivial cases
    if(sum(tc) == 0) return(matrix(0, nrow = 8, ncol = 2))
    if(all(dim(tc) == 1)) return(matrix(c(tc[1, 1], rep(0, 15)),
                                        nrow = 8, ncol = 2))
    # Which increments in pw excl event counts (nondecreasing) are positive
    i2l <- sapply(0:(nrow(tc) - 1), function(i) {
      lambda <- index_partition(i)
      ed <- c(0, lambda[3:2]) < lambda[3:1]
      return(sum((2 ^ (2:0)) * ed) + 1)
    })
    wh <- lapply(1:8, function(i) which(i2l == i))
    matrix(c(
      # Empty triads
      tc[1, 1],
      # No triad event; 1, 2, and 3 pairs connected
      sapply(2:8, function(i) sum(tc[wh[[i]], 1])),
      # Committees with no pairwise events
      sum(tc[1, -1]),
      # Triad event; 1, 2, and 3 pairs connected
      sapply(2:8, function(i) sum(tc[wh[[i]], -1]))
    ), nrow = 8, ncol = 2)
  }
