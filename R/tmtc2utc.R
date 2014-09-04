#' Two-mode triad census to uniformity triad census
#' 
#' @param tmtc A full two-mode triad census (not necessarily labeled)
#' @export

tmtc2utc <-
function(tmtc) {
        # Trivial cases
        if(sum(tmtc) == 0) return(matrix(0, nr = 8, nc = 2))
        if(all(dim(tmtc) == 1)) return(matrix(c(tmtc[1, 1], rep(0, 15)),
                                              nr = 8, nc = 2))
        # Which increments in pw excl event counts (nondecreasing) are positive
        i2l <- sapply(0:(nrow(tmtc) - 1), function(i) {
            lambda <- index.partition(i)
            ed <- c(0, lambda[3:2]) < lambda[3:1]
            return(sum((2 ^ (2:0)) * ed) + 1)
        })
        wh <- lapply(1:8, function(i) which(i2l == i))
        return(matrix(c(
            # Empty triads
            tmtc[1, 1],
            # No triad event; 1, 2, and 3 pairs connected
            sapply(2:8, function(i) sum(tmtc[wh[[i]], 1])),
            # Committees with no pairwise events
            sum(tmtc[1, 2:ncol(tmtc)]),
            # Triad event; 1, 2, and 3 pairs connected
            sapply(2:8, function(i) sum(tmtc[wh[[i]], 2:ncol(tmtc)]))
        ), nr = 8, nc = 2))
    }
