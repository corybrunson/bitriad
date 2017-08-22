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
#'   \code{\link{triad_census_an}} (called from \code{triad_census} when the 
#'   \code{graph} argument is an \code{affiliation_network}) defaults to this
#'   census.

#'   \item For the analysis of sparse affiliation networks, the full triad 
#'   census may be less useful than information on whether the extent of 
#'   connectivity through co-attended events differs between each pair of 
#'   actors. In order to summarize this information, a coarser triad census can 
#'   be conducted on classes of triads based on the following congruence 
#'   relation: Using the indices \eqn{\lambda=(x\geq y\geq z)} and \eqn{w} 
#'   above, note that the numbers of shared events for each pair and for the 
#'   triad are \eqn{x+w\geq y+w\geq z+w\geq w\geq 0}. Consider two triads 
#'   congruent if the same subset of these weak inequalities are strictly 
#'   satisfied. The resulting \emph{difference triad census}, previously called 
#'   the \emph{uniformity triad census}, implemented as
#'   \code{\link{triad_census_difference}}, is organized into a \eqn{8\times 2} 
#'   matrix with the strictness of the first three inequalities determining the 
#'   row and that of the last inequality determining the column.

#'   \item A still coarser congruence relation can be used to tally how many are
#'   connected by at least one event in each distinct way. This relation 
#'   considers two triads congruent if each corresponding pair of actors both 
#'   attended or did not attend at least one event not attended by the third, 
#'   and if the corresponding triads both attended or did not attend at least 
#'   one event together. The \emph{binary triad census} (Brunson, 2015; therein 
#'   called the \emph{structural triad census}), implemented as 
#'   \code{\link{triad_census_binary}}, records the number of triads in each 
#'   congruence class.

#'   \item The \emph{simple triad census} is the 4-entry triad census on a 
#'   traditional (non-affiliation) network indicating the number of triads of 
#'   each isomorphism class, namely whether the triad contains zero, one, two, 
#'   or three links. The function \code{\link{simple_triad_census}} computes the
#'   classical (undirected) triad census for an undirected traditional network, 
#'   or for the actor projection of an affiliation network (if provided), using 
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
#'   is adapted from the algorithm of Batagelj and Mrvar (2001) for calculating
#'   the classical triad census for a directed graph.
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
