#' Uniformity triad census to simple triad census
#' 
#' @param utc A uniformity two-mode triad census (not necessarily labeled)
#' @export

utc2stc <- function(utc) ctc2stc(utc2ctc(utc))
