#' Two-mode triad census to simple triad census
#' 
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
