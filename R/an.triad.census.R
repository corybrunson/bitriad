#' Affiliation network (full) triad census
#'
#' This function computes the full triad census for an affiliation network.
#' @param bigraph An affiliation network.
#' @param rcnames Logical; whether to label the matrix rows and columns
#' @param verbose Logical; whether to display progress bars
#' @export
#' @examples
#' data(ddggs.clique)
#' tc <- an.triad.census(ddggs.clique, rcnames = TRUE)
#' tc
#' sum(tc) == choose(vcount(actor.projection(ddggs.clique)), 3)

an.triad.census <-
    function(bigraph, rcnames = FALSE, verbose = FALSE) {

        # Check that bigraph is an affiliation network
        if(!is.an(bigraph)) stop('Not an affiliation network')

        # Drop trivial cases
        if(vcount(bigraph) == 0) return(matrix(0, nr = 0, nc = 0))
        # Create projection
        graph <- actor.projection(bigraph, name = 'id')
        # Trivial case
        if(ecount(graph) == 0) {
            C <- matrix(choose(vcount(graph), 3), nr = 1, nc = 1)
            if(rcnames) {
                row.names(C) <- '(0,0,0)'
                col.names(C) <- '0'
            }
            return(C)
        }

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
        tht <- three.tied.triads(bigraph, graph = graph)
        # If there are any...
        if(!is.null(tht)) {
            # Trim any unnecessary columns
            max.w <- max(tht$w)
            C <- C[, 1:(max.w + 1), drop = FALSE]
            # For each value of w:
            for(w in 0:max.w) {
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
        # (but only makes sense within range of 'numeric' accuracy)
        # http://stackoverflow.com/questions/8804779/
        # what-is-integer-overflow-in-r-and-how-can-it-happen
        stopifnot(sum(C) == choose(vcount(graph), 3))
        # Clear names
        colnames(C) <- NULL
        if(rcnames) {
            colnames(C) <- 0:(ncol(C) - 1)
            rownames(C) <- sapply(0:(nrow(C) - 1), function(i) paste(
                '(', paste(index.partition(i), collapse = ','),
                ')', sep = ''))
        }
        as.matrix(C)
    }
