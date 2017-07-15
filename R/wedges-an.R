#' @title Wedge censuses and closure indicators for affiliation networks
#'   
#' @description Given an affiliation network and a vector of node IDs, list all 
#'   wedges of a specified type centered at each node and indicate whether each 
#'   is closed.
#'   
#' @details Each measure of transitive closure implemented here is defined as 
#'   the proportion of wedges that are closed, where a \emph{wedge} is the image
#'   of a specified self-dual two-event triad \eqn{W} under a specified 
#'   subcategory of graph maps \eqn{\mathcal{C}} subject to a specified 
#'   congruence relation \eqn{\tilde}, and where a wedge is \emph{closed} if it 
#'   is the image of such a map that factors through a canonical inclusion of 
#'   \eqn{W} to a specified self-dual three-event triad \eqn{X}.
#'   
#'   The alcove, wedge, maps, and congruence can be specified by numerical codes
#'   as follows:
#'   \itemize{
#'    \item{"alcove:"}{
#'     \itemize{0}{\eqn{T_{(1,1,1),0}}}
#'     \itemize{1}{\eqn{T_{(1,1,0),1}}}
#'     \itemize{2}{\eqn{T_{(1,0,0),2}}}
#'     \itemize{3}{\eqn{T_{(0,0,0),3}}}
#'    }
#'    \item{"wedge:"}{
#'     \itemize{0}{\eqn{T_{(1,1,0),0}}}
#'     \itemize{1}{\eqn{T_{(1,0,0),1}}}
#'     \itemize{2}{\eqn{T_{(0,0,0),2}}}
#'    }
#'    \item{"maps:"}{
#'     \itemize{0}{all graph maps (injective on actors)}
#'     \itemize{1}{injective graph maps}
#'     \itemize{2}{induced injective graph maps}
#'    }
#'    \item{"congruence:"}{
#'     \itemize{0}{same actor images}
#'     \itemize{1}{same actor images, structurally equivalent event images}
#'     \itemize{2}{same actor images, same event images}
#'    }
#'   }
#' 
#' See the references for a full definition.
#' 
#' @references Brunson, J.C. (2015). Triadic analysis of affiliation networks. 
#'   *Network Science*, 3 (4), 480--508.
#'   
#' @name wedges_an
#' @param bigraph An affiliation network.
#' @param q An actor node in \code{bigraph}.
#' @param alcove,wedge,maps,congruence Choice of alcove, wedge, maps, and
#'   congruence (see Details).
#' @family wedge functions
#' @export
wedges_an <- function(
  bigraph, q,
  alcove = 0, wedge = 0, maps = 0, congruence = 0
) {
  suffix <- paste0(
    "x", alcove,
    "w", wedge,
    "m", maps,
    "c", congruence
  )
  wedges_fun <- get(paste0("wedges_", suffix))
  wedges_fun(
    el = as_edgelist(bigraph, names = FALSE),
    q = q
  )
}
