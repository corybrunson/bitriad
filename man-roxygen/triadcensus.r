#' @details
#' 

#' @section Triad censuses: Three triad censuses are implemented for affiliation 
#'   networks:
#'   \itemize{

#'   \item The \emph{full triad census} (Brunson, 2015) records the number of 
#'   triads of each isomorphism class. The classes are indexed by a partition, 
#'   \eqn{\lambda=(\lambda_1\leq\lambda_2\leq\lambda_3)}, indicating the number 
#'   of events attended by both actors in each pair but not the third, and a 
#'   positive integer, \eqn{w}, indicating the number of events attended by all 
#'   three actors. The isomorphism classes are organized into a matrix with rows
#'   indexed by \eqn{\lambda} and columns indexed by \eqn{w}, with the 
#'   partitions \eqn{\lambda} ordered according to the \emph{revolving door
#'   ordering} (Kreher & Stinson, 1999). The main function
#'   \code{\link{triad_census_an}} defaults to this census.

#'   \item In the context of sparse affiliation networks, the full triad census 
#'   may be less useful than information on how many triads are connected by at 
#'   least one event in each distinct way. Conceptually, a congruence relation 
#'   can be defined on the set of triads that considers any two triads congruent
#'   if each corresponding pair of actors both attended or did not attend at 
#'   least one event not attended by the third, and if the corresponding triads 
#'   both attended or did not attend at least one event together. The 
#'   \emph{binary triad census} (Brunson, 2015; therein called the
#'   \emph{structural triad census}) records the number of triads in each
#'   congruence class.

#'   \item The \emph{simple triad census} is just the traditional 4-entry triad 
#'   census on the netwok projected onto the actor nodes. The function 
#'   \code{\link{simple_triad_census}} computes the classical (undirected) triad
#'   census for the actor projection of an affiliation network, using 
#'   \code{\link[igraph]{triad_census}}; if the result doesn't make sense (i.e.,
#'   the sum of the entries is not the number of triples of nodes), then it 
#'   instead uses its own, much slower method.

#'   }

#'   Each of these censuses can be projected from the previous using the 
#'   function \code{\link{project_census}}. A fourth census, called the 
#'   \emph{uniformity triad census} and implemented as 
#'   \code{\link{unif_triad_census}}, is deprecated. Three-actor triad 
#'   affiliation networks can be constructed and plotted using the 
#'   \code{\link{triad}} functions.
#'   

#'   The default method for the two affiliation network--specific triad censuses
#'   is adapted from the algorithm of Batagelj and Mrvar (2001).
#'   

#' @references
#' 
#' Kreher, D.L., & Stinson, D.R. (1999). Combinatorial algorithms: generation, 
#' enumeration, and search. \emph{SIGACT News}, 30(1), 33--35.
#' 
#' Batagelj, V., & Mrvar, A. (2001). A subquadratic triad census algorithm for 
#' large sparse networks with small maximum degree. \emph{Social Networks},
#' 23(3), 237--243.
#' 
#' Brunson, J.C. (2015). Triadic analysis of affiliation networks. \emph{Network
#' Science}, 3(4), 480--508.
#' 
