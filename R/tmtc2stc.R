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

tmtc2stc <-
function(tmtc) {
        # Trivial cases
        if(sum(tmtc) == 0) return(rep(0, 4))
        if(all(dim(tmtc) == 1)) return(c(tmtc[1, 1], 0, 0, 0))
        # Number of edges (1, 2, or 3) induced by each lambda except c(0, 0, 0)
        pw.counts <- sapply(1:(dim(tmtc)[1] - 1), function(i) {
            length(which(index.partition(i) > 0))
        })
        return(c(
            # Empty triads all have lambda = c(0, 0, 0), w = 0
            tmtc[1, 1],
            # Dyads
            sum(tmtc[which(pw.counts == 1) + 1, 1]),
            # Intransitive wedges
            sum(tmtc[which(pw.counts == 2) + 1, 1]),
            # Triangles, including all columns w > 0
            sum(tmtc[which(pw.counts == 3) + 1, 1]) +
                ifelse(dim(tmtc)[2] == 1, 0, sum(tmtc[, 2:dim(tmtc)[2]]))))
    }
