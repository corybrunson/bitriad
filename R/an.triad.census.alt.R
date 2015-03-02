#' Affiliation network (full) triad census
#'
#' These functions compute triad censuses for affiliation networks. (Currently
#' only the full triad census is implemented and the structural and simple
#' censuses are computed from it.) The full and structural censuses return
#' matrices, the former of whatever dimension is needed to contain the census
#' and the latter 4-by-2. The simple census returns a vector of length 4.
#' @param bigraph The ambient affiliation network
#' @param rcnames Logical; whether to label the matrix rows and columns
#' @param verbose Logical; whether to display progress bars

an.triad.census.alt <-
    function(bigraph, rcnames = FALSE, verbose = FALSE) {
        # Drop trivial cases
        if(vcount(bigraph) == 0) return(matrix(0, nr = 0, nc = 0))
        # Create one-mode projection
        graph <- actor.projection(bigraph, name = 'id')

        # Find maximum values of x and of w
        max.x <- max(E(graph)$weight)
        # Initialize matrix (overestimating the number of columns)
        C <- as.data.frame(matrix(0, nr = choose(max.x + 3, 3), nc = max.x + 1))

        # Tally one-tied triads
        ot <- one.tied.triads(graph)
        # Insert the totals at the proper entries of C
        # (No repeats, so no information loss)
        C[sapply(ot$x, function(x) partition.index(c(x, 0, 0))) + 1, 1] <- ot$n
        if(verbose) print('One-tied triads tallied')

        # Tally connected triples
        ct <- connected.triples(bigraph, graph = graph)
        # Trim any unnecessary columns
        max.w <- max(ct$w)
        C <- C[, 1:(max.w + 1)]
        # For each value of w:
        for(w in 0:max.w) {
            if(verbose) print(paste('Tallying weight-', w, ' connected triples',
                                    sep = ''))
            # Which rows have weight w?
            rs <- which(ct$w == w)
            # Insert the totals at the proper rows in column w + 1 of C
            # (No repeats, so no information loss)
            C[sapply(rs, function(i) {
                partition.index(as.numeric(ct[i, 1:3])) + 1
            }), w + 1] <- ct$n[rs]
        }
        if(verbose) print('Connected triples tallied')

        # The remaining triads share no secondary nodes; count them as empty
        # (No triads should have yet been counted as empty)
        C[1, 1] <- choose(vcount(graph), 3) - sum(C)
        # Clear names
        colnames(C) <- NULL
        if(rcnames) {
            colnames(C) <- 0:(ncol(C) - 1)
            rownames(C) <- sapply(0:(nrow(C) - 1), function(i) paste(
                '(', paste(index.partition(i, k = 3), collapse = ','),
                ')', sep = ''))
        }
        as.matrix(C)
    }
