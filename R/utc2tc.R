#' Uniformity triad census to simple triad census
#' 
#' @param tc A uniformity triad census (not necessarily labeled)

utc2tc <- function(tc) stc2tc(utc2stc(tc))
