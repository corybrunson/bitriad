#' @title Wedge censuses and closure indicators for affiliation networks
#'   
#' @description Given an affiliation network and a node ID, identify all wedges 
#'   of a specified flavor centered at the node and indicate whether each is 
#'   closed.
#'   
#' @details Each measure of transitive closure implemented here is defined as 
#'   the proportion of wedges that are closed, where a \emph{wedge} is the image
#'   of a specified two-event triad \eqn{W} under a specified subcategory of 
#'   graph maps \eqn{C} subject to a specified congruence relation \eqn{~}, and 
#'   where a wedge is \emph{closed} if it is the image of such a map that 
#'   factors through a canonical inclusion of \eqn{W} to a specified self-dual 
#'   three-event triad \eqn{X}.
#'   

#' The alcove, wedge, maps, and congruence can be specified by numerical codes
#' as follows:
#' \itemize{
#'  \item{\code{alcove}:
#'   \itemize{
#'    \item{\code{0}: \eqn{T_{(1,1,1),0}}}
#'    \item{\code{1}: \eqn{T_{(1,1,0),1}}}
#'    \item{\code{2}: \eqn{T_{(1,0,0),2}}}
#'    \item{\code{3}: \eqn{T_{(0,0,0),3}}}
#'   }
#'  }
#'  \item{\code{wedge}:
#'   \itemize{
#'    \item{\code{0}: \eqn{T_{(1,1,0),0}}}
#'    \item{\code{1}: \eqn{T_{(1,0,0),1}}}
#'    \item{\code{2}: \eqn{T_{(0,0,0),2}}}
#'   }
#'  }
#'  \item{\code{maps}:
#'   \itemize{
#'    \item{\code{0}: all graph maps (injective on actors)}
#'    \item{\code{1}: injective graph maps}
#'    \item{\code{2}: induced injective graph maps}
#'   }
#'  }
#'  \item{\code{congruence}:
#'   \itemize{
#'    \item{\code{0}: same actor and event images (equivalence)}
#'    \item{\code{1}: same actor images, structurally equivalent event images}
#'    \item{\code{2}: same actor images}
#'   }
#'  }
#' }
#' See Brunson (2015) for a full definition.
#' 

#' Some specifications correspond to statistics of especial interest:
#' \itemize{
#'  \item{\code{0,0,0,2}:
#'   the classical clustering coefficient (Watts & Strogatz, 1998),
#'   evaluated on the unipartite actor projection
#'  }
#'  \item{\code{0,0,1,0}:
#'   the two-mode clustering coefficient (Opsahl, 2013)
#'  }
#'  \item{\code{0,0,2,0}:
#'   the unconnected clustering coefficient (Liebig & Rao, 2014)
#'  }
#'  \item{\code{3,2,2,0}:
#'   the completely clustering coefficient (Liebig & Rao, 2014)
#'   (\strong{not yet implemented})
#'  }
#'  \item{\code{0,0,2,1}:
#'   the exclusive clustering coefficient (Brunson, 2015)
#'  }
#'  \item{\code{0,0,2,2}:
#'   the exclusive clustering coefficient
#'  }
#' }
#' See the references for discussions of each.
#' 

#' @references
#' 
#' Watts, D.J., & Strogatz, S.H. (1998). Collective dynamics of "small-world" 
#' networks. \emph{Nature}, 393(6684), 440--442.
#' 
#' Opsahl, T. (2013). Triadic closure in two-mode networks: Redefining the 
#' global and local clustering coefficients. \emph{Social Networks}, 35(2),
#' 159--167. Special Issue on Advances in Two-mode Social Networks.
#' 
#' Liebig, J., & Rao, A. (2014). Identifying influential nodes in bipartite 
#' networks using the clustering coefficient. Pages 323--330 of:
#' \emph{Proceedings of the tenth international conference on signal-image
#' technology and internet-based systems}.
#' 
#' Brunson, J.C. (2015). Triadic analysis of affiliation networks. \emph{Network
#' Science}, 3(4), 480--508.
#' 

#' @name wedges_an
#' @family wedge functions
#' @param bigraph An affiliation network.
#' @param actor An actor node in \code{bigraph}.
#' @param alcove,wedge,maps,congruence Choice of alcove, wedge, maps, and 
#'   congruence (see Details).
#' @return A two-element list consisting of (1) a 3- or 5-row integer matrix of 
#'   (representatives of) all (congruence classes of) wedges in \code{bigraph} 
#'   centered at \code{actor}, and (2) a logical vector indicating whether each 
#'   wedge is closed.
#' @export
wedges_an <- function(
  bigraph, actor,
  alcove = 0, wedge = 0, maps = 0, congruence = 0
) {
  stopifnot(V(bigraph)[actor]$type == FALSE)
  suffix <- paste0(
    "x", alcove,
    "w", wedge,
    "m", maps,
    "c", congruence
  )
  wedges_fun <- get(paste0("wedges_", suffix))
  wedges_fun(
    el = as_edgelist(bigraph, names = FALSE),
    q = as.numeric(V(bigraph)[actor])
  )
}

wedges_x0w0m2c2 <- wedges_x0w0m2c1

#' @rdname wedges_an
#' @export
wedges_an_watts_strogatz <- function(bigraph, actor) wedges_an(
  bigraph = bigraph, actor = actor,
  alcove = 0, wedge = 0, maps = 0, congruence = 2
)

#' @rdname wedges_an
#' @export
wedges_an_classical <- wedges_an_watts_strogatz

#' @rdname wedges_an
#' @export
wedges_an_opsahl <- function(bigraph, actor) wedges_an(
  bigraph = bigraph, actor = actor,
  alcove = 0, wedge = 0, maps = 1, congruence = 0
)

#' @rdname wedges_an
#' @export
wedges_an_twomode <- wedges_an_opsahl

#' @rdname wedges_an
#' @export
wedges_an_liebig_rao_0 <- function(bigraph, actor) wedges_an(
  bigraph = bigraph, actor = actor,
  alcove = 0, wedge = 0, maps = 2, congruence = 0
)

#' @rdname wedges_an
#' @export
wedges_an_unconnected <- wedges_an_liebig_rao_0

#' @rdname wedges_an
#' @export
wedges_an_liebig_rao_3 <- function(bigraph, actor) wedges_an(
  bigraph = bigraph, actor = actor,
  alcove = 3, wedge = 2, maps = 2, congruence = 0
)

#' @rdname wedges_an
#' @export
wedges_an_completely_connected <- wedges_an_liebig_rao_3

#' @rdname wedges_an
#' @export
wedges_an_exclusive <- function(bigraph, actor) wedges_an(
  bigraph = bigraph, actor = actor,
  alcove = 0, wedge = 0, maps = 2, congruence = 1
)
