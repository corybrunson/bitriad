#' @details
#' 

#' @section Triad census: The function \code{\link{triad_census_an}} computes 
#'   the full triad census for an affiliation network. The functions 
#'   \code{\link{unif_triad_census}} and \code{\link{str_triad_census}} compute 
#'   the more compact uniformity and structural triad censuses, respectively, 
#'   using similar methods. The function \code{\link{simple_triad_census}} 
#'   computes the classical (undirected) triad census for the actor projection 
#'   of an affiliation network, using \code{\link[igraph]{triad_census}} but, if
#'   the result doesn't make sense (i.e. the sum of the entries is not the 
#'   number of triples of nodes), then it instead uses its own, much slower 
#'   method. Each of these censuses can be projected from the previous using the
#'   function \code{\link{project_census}}.
#'   
