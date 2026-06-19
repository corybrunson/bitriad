#' @section Measures of triad closure: Each measure of triad closure is defined 
#'   as the proportion of wedges that are closed, where a *wedge* is the 
#'   image of a specified two-event triad \eqn{W} under a specified subcategory 
#'   of graph maps \eqn{C} subject to a specified congruence relation \eqn{~}, 
#'   and where a wedge is *closed* if it is the image of such a map that 
#'   factors through a canonical inclusion of \eqn{W} to a specified self-dual 
#'   three-event triad \eqn{X}.
#'   

#' The alcove, wedge, maps, and congruence can be specified by numerical codes
#' as follows (no plans exist to implement more measures than these):
#' \itemize{
#'  \item{`alcove`:
#'   \itemize{
#'    \item{`0`: \eqn{T_{(1,1,1),0}}}
#'    \item{`1`: \eqn{T_{(1,1,0),1}}} (**not yet implemented**)
#'    \item{`2`: \eqn{T_{(1,0,0),2}}} (**not yet implemented**)
#'    \item{`3`: \eqn{T_{(0,0,0),3}}} (**not yet implemented**)
#'   }
#'  }
#'  \item{`wedge`:
#'   \itemize{
#'    \item{`0`: \eqn{T_{(1,1,0),0}}}
#'    \item{`1`: \eqn{T_{(1,0,0),1}}} (**not yet implemented**)
#'    \item{`2`: \eqn{T_{(0,0,0),2}}} (**not yet implemented**)
#'   }
#'  }
#'  \item{`maps`:
#'   \itemize{
#'    \item{`0`: all graph maps (injective on actors)}
#'    \item{`1`: injective graph maps}
#'    \item{`2`: induced injective graph maps}
#'   }
#'  }
#'  \item{`congruence`:
#'   \itemize{
#'    \item{`0`: same actor and event images (equivalence)}
#'    \item{`1`: same actor images, structurally equivalent event images}
#'    \item{`2`: same actor images}
#'   }
#'  }
#' }
#' Some specifications correspond to statistics of especial interest:
#' \itemize{
#'  \item{`0,0,0,2`:
#'   the classical clustering coefficient (Watts & Strogatz, 1998),
#'   evaluated on the unipartite actor projection
#'  }
#'  \item{`0,0,1,0`:
#'   the two-mode clustering coefficient (Opsahl, 2013)
#'  }
#'  \item{`0,0,2,0`:
#'   the unconnected clustering coefficient (Liebig & Rao, 2014)
#'  }
#'  \item{`3,2,2,0`:
#'   the completely connected clustering coefficient (Liebig & Rao, 2014)
#'   (**not yet implemented**)
#'  }
#'  \item{`0,0,2,1`:
#'   the exclusive clustering coefficient (Brunson, 2015)
#'  }
#'  \item{`0,0,2,2`:
#'   the exclusive clustering coefficient
#'  }
#' }
#' See Brunson (2015) for a general definition and the aforecited references for
#' discussions of each statistic.
#' 

#' @references
#' 
#' Watts, D.J., & Strogatz, S.H. (1998). Collective dynamics of "small-world" 
#' networks. *Nature*, 393(6684), 440--442.
#' 
#' Opsahl, T. (2013). Triadic closure in two-mode networks: Redefining the 
#' global and local clustering coefficients. *Social Networks*, 35(2),
#' 159--167. Special Issue on Advances in Two-mode Social Networks.
#' 
#' Liebig, J., & Rao, A. (2014). Identifying influential nodes in bipartite 
#' networks using the clustering coefficient. Pages 323--330 of:
#' *Proceedings of the tenth international conference on signal-image
#' technology and internet-based systems*.
#' 
#' Brunson, J.C. (2015). Triadic analysis of affiliation networks. *Network
#' Science*, 3(4), 480--508.
#' 
