#' @title \code{bitriad}: Triadic analysis of affiliation networks
#'   
#' @description Calculate triad censuses and triad closure statistics designed 
#'   for affiliation networks.
#'   
#' @details The package contains two principal tools for the triadic analysis of
#'   affiliation networks: triad censuses and triad closure statistics. Assorted
#'   additional functions, including a measure of dynamic triad closure, are
#'   also included.
#'   
#' @template triadcensus
#' @template triadclosure
#'   
#' @docType package
#' @author Jason Cory Brunson
#' @import MASS igraph Rcpp
#' @importFrom Rcpp evalCpp
#' @useDynLib bitriad
#' @name bitriad
NULL
