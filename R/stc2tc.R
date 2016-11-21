#' Structural triad census to simple triad census
#' 
#' @param tc A structural triad census (not necessarily labeled); 4 by 2 matrix
stc2tc <-
  function(tc) {
    c(
      tc[1:3, 1],
      sum(c(tc[4, 1], tc[1:4, 2]))
    )
  }
