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

simple.triad.census <-
function(graph, rcnames = FALSE) {
        tc <- triad.census(as.directed(graph))
        stopifnot(sum(tc) == choose(vcount(graph), 3))
        if(is.nan(tc[1])) {
            tc[1] <- choose(vcount(graph), 3) - sum(tc, na.rm = TRUE)
        }
        stc <- tc[c(1, 3, 11, 16)]
        if(rcnames) names(stc) <- 0:3
        return(stc)
    }
