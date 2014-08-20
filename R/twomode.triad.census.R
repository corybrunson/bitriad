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

twomode.triad.census <-
function(bigraph, type = 0, rcnames = FALSE,
             verbose = FALSE) {
        # Drop trivial cases
        if(vcount(bigraph) == 0) return(matrix(0, nr = 0, nc = 0))
        # Create one-mode projection
        graph <- onemode.projection(bigraph, type = type, name = 'id')
        # Trivial case
        if(ecount(graph) == 0) return(matrix(choose(vcount(graph), 3),
                                             nr = 1, nc = 1))
        
        # Find maximum values of x and of w
        max.x <- max(E(graph)$weight)
        # Initialize matrix (overestimating the number of columns)
        C <- as.data.frame(matrix(0, nr = choose(max.x + 3, 3), nc = max.x + 1))
        
        # Tally one-tied triads
        ot <- one.tied.triads(graph)
        # Insert the totals at the proper entries of C
        # (Aggregated, so no repeats, so no information loss)
        if(length(ot) > 0) C[sapply(ot$x, function(x) {
            partition.index(c(x, 0, 0))
        }) + 1, 1] <- ot$n
        if(verbose) print('One-tied triads tallied')
        
        # Tally two-tied triads
        tt <- two.tied.triads(graph)
        # Insert the totals at the proper entries of C
        # (Aggregated, so no repeats, so no information loss)
        if(!is.null(tt)) C[sapply(1:dim(tt)[1], function(i) {
            partition.index(c(tt[i, 1], tt[i, 2], 0))
        }) + 1, 1] <- tt$n
        if(verbose) print('Two-tied triads tallied')
        
        # Tally triangles
        tht <- three.tied.triads(bigraph, type = type, graph = graph)
        # If there are any...
        if(!is.null(tht)) {
            # Trim any unnecessary columns
            max.w <- max(tht$w)
            C <- C[, 1:(max.w + 1)]
            # For each value of w:
            for(w in 0:max.w) {
                if(verbose) print(paste('Tallying weight-', w,
                                        ' three-tied triads', sep = ''))
                # Which rows have weight w?
                rs <- which(tht$w == w)
                # Insert the totals at the proper rows in column w + 1 of C
                # (No repeats, so no information loss)
                if(length(rs) > 0) C[sapply(rs, function(i) {
                    partition.index(as.numeric(tht[i, 1:3])) + 1
                }), w + 1] <- tht$n[rs]
            }
        }
        if(verbose) print('Three-tied triads tallied')
        
        # The remaining triads share no secondary nodes; count them as empty
        # (No triads should have yet been counted as empty)
        C[1, 1] <- choose(vcount(graph), 3) - sum(C)
        # Reality check: The total triad tally should equal |V(graph)|-choose-3
        stopifnot(sum(C) == choose(vcount(graph), 3))
        # Clear names
        colnames(C) <- NULL
        if(rcnames) {
            colnames(C) <- 0:(ncol(C) - 1)
            rownames(C) <- sapply(0:(nrow(C) - 1), function(i) paste(
                '(', paste(index.partition(i, k = 3), collapse = ','),
                ')', sep = ''))
        }
        return(as.matrix(C))
    }
