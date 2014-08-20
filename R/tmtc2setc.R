#' Two-mode triad census
#' 
#' These functions compute triad censuses for affiliation networks. (Currently
#' only the full triad census is implemented and the structural and simple
#' censuses are computed from it.) The full and structural censuses return
#' matrices, the former of whatever dimension is needed to contain the census
#' and the latter 4-by-2. The simple census returns a vector of length 4.
#' @param bigraph The ambient affiliation network
#' @param graph A one-mode network (used as the one-mode projection of bigraph)
#' @param type The actor node type in bigraph
#' @param rcnames Logical; whether to label the matrix rows and columns
#' @param verbose Logical; whether to display progress bars
#' @param tmtc A full two-mode triad census (not necessarily labeled)
#' @export

tmtc2setc <-
function(tmtc) {
        # Trivial cases
        if(sum(tmtc) == 0) return(matrix(0, nr = 4, nc = 2))
        if(all(dim(tmtc) == 1)) return(matrix(c(tmtc[1, 1], rep(0, 7)),
                                              nr = 4, nc = 2))
        # No. edges (1, 2, or 3) induced by each nonempty lambda with w = 0
        pw.counts <- sapply(1:(dim(tmtc)[1] - 1), function(i) {
            length(which(position.index(i, k = 3) > 0))
        })
        # Which rows connect 1, 2, and 3 pairs
        wh <- lapply(1:3, function(i) which(pw.counts == i) + 1)
        return(matrix(c(
            # Empty triads
            tmtc[1, 1],
            # No triad event; 1, 2, and 3 pairs connected
            sum(tmtc[wh[[1]], 1]),
            sum(tmtc[wh[[2]], 1]),
            sum(tmtc[wh[[3]], 1]),
            # Committees with no pairwise events
            sum(tmtc[1, 2:dim(tmtc)[2]]),
            # Triad event; 1, 2, and 3 pairs connected
            sum(tmtc[wh[[1]], 2:dim(tmtc)[2]]),
            sum(tmtc[wh[[2]], 2:dim(tmtc)[2]]),
            sum(tmtc[wh[[3]], 2:dim(tmtc)[2]])
        ), nr = 4, nc = 2))
    }
