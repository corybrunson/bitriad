#' Affiliation network clustering coefficients
#' 
#' This function computes a given flavor of transitivity (triadic closure) on a
#' given affiliation network. The calculations are performed locally. Each
#' flavor is defined as a proportion of "wedges" that are "closed", for suitable
#' definitions of both terms. The function `transitivity_an` is a shell that
#' proceeds across actors and computes wedges using the provided `wedgeFun`.
#' These functions count the "wedges", and among them the "closed" ones,
#' centered at a given actor node in a given affiliation network.
#' @param bigraph An affiliation network; see \code{is_an}.
#' @param type Character; the type of clustering coefficient (defaults to
#' "global").
#' @param wedgeFun The wedge function.
#' @param vids A subset of actor node ids at which to evaluate the local
#' clustering coefficient.
#' @param add.names Logical; whether to label the matrix rows and columns.
#' @param Q An actor node in \code{bigraph}.
#' @return If \code{type} is "global", the global clustering coefficient of the
#' network; if "local", the local clustering coefficients of the actors;
#' otherwise, a 2-column matrix, each row of which gives the number of wedges
#' and the number of closed wedges centered at each actor.
#' @export
#' @examples
#' data(women_clique)
#' sapply(c(injequ_wedges, injstr_wedges, indstr_wedges),
#'          transitivity_an, bigraph = women_clique, type = "local")
#' data(women_group)
#' excl_transitivity(women_group)
#' cbind(
#'     project_transitivity(women_group, type = "local"),
#'     opsahl_transitivity(women_group, type = "local"),
#'     excl_transitivity(women_group, type = "local")
#' )

transitivity_an <-
    function(
        bigraph,
        type = "global",
        wedgeFun = injequ_wedges,
        vids = which(!V(bigraph)$type), add.names = FALSE
    ) {
        if(vcount(bigraph) == 0) {
            if(type == "global") {
                return(NaN)
            } else if(type == "local") {
                return(NULL)
            } else return(matrix(NA, nr = 0, nc = 2))
        }
        # Check that nodes are actors
        stopifnot(all(!V(bigraph)$type[vids]))
        # If global or both, need to look at all vertices
        Qs <- if(type == "global") which(!V(bigraph)$type) else vids
        # Array of 4-paths centered at each Q in Qs
        wedges <- matrix(unlist(lapply(Qs, function(Q) {
            # Return wedge and closed wedge counts at Q
            wedgeFun(bigraph, Q)
        })), nr = 2)
        if(type == "global") {
            return(sum(wedges[2, ]) / sum(wedges[1, ]))
        }
        if(type == "local") return(wedges[2, ] / wedges[1, ])
        wedges <- t(wedges)
        if(add.names) {
            rownames(wedges) <- V(bigraph)$name[vids]
            colnames(wedges) <- c("Wedges", "Closed")
        }
        wedges
    }

indequ_wedges <-
    function(bigraph, Q) {
        # Identify secondary neighbors of Q
        n1 <- setdiff(neighborhood(bigraph, 1, Q)[[1]], Q)
        # If there aren't at least two, return zeroes
        if(length(n1) < 2) return(c(0, 0))
        # Identify primary neighborhoods of secondary neighbors of Q
        n1n1 <- lapply(neighborhood(bigraph, 1, n1), setdiff, c(Q, n1))
        # Array the 2-paths centered at Q
        # (Note that these are indices of n1, not vertex ids)
        p <- utils::combn(1:length(n1), 2)
        # Across the pairs (X, Y) list the numbers of wedges and closed wedges
        wedgelist <- do.call(cbind, lapply(1:ncol(p), function(j) {
            # The first node X must have a nonempty neighborhood besides Q
            # and at least one neighbor not tied to Y (= n1[p[2, j]])
            Ps <- setdiff(n1n1[[p[1, j]]], n1n1[[p[2, j]]])
            if(length(Ps) == 0) return(c(0, 0))
            # Across all choices of P from the non-Q primary neighbors of X
            # that are not tied to Y
            do.call(cbind, lapply(Ps, function(P) {
                # Y must have a nonempty nbhd besides Q and P,
                # from which R, which cannot be tied to X, is to be drawn
                Rs <- setdiff(n1n1[[p[2, j]]], c(P, n1n1[[p[1, j]]]))
                if(length(Rs) == 0) return(c(0, 0))
                # Which Rs produce 4-paths (P, X, Q, Y, R) that are closed
                # by events Z that are not tied to Q?
                Rw <- which(sapply(Rs, function(R) {
                    length(setdiff(
                        intersect(neighborhood(bigraph, 1, P)[[1]],
                                  neighborhood(bigraph, 1, R)[[1]]),
                        n1)) > 0
                }))
                return(c(length(Rs), length(Rw)))
            }))
        }))
        rowSums(wedgelist)
    }

indequ_transitivity <-
    function(
        bigraph, type = "global",
        vids = which(!V(bigraph)$type)
    ) {
        transitivity_an(
            bigraph = bigraph, type = type,
            wedgeFun = indequ_wedges, vids = vids)
    }

injstr_wedges <-
    function(bigraph, Q) {
        # Identify nodes of separation (exactly) 1 and 2 from Q
        n1 <- setdiff(neighborhood(bigraph, 1, Q)[[1]], Q)
        n2 <- setdiff(neighborhood(bigraph, 2, Q)[[1]], c(n1, Q))
        # Identify events attended by these actors
        n2n1 <- lapply(neighborhood(bigraph, 1, n2), setdiff, y = n2)
        
        # Require at least two nodes of separation 2 for a wedge
        if(length(n2) < 2) return(c(0, 0))
        # Identify pairs (P, R) of nodes in n2
        p <- utils::combn(length(n2), 2)
        
        # Count the structurally distinct wedges (0 thru 4) for each pair (P, R)
        # and the number of these that are closed
        wedgelist <- sapply(1:ncol(p), function(j) {
            # Whether P and Q share an exclusive event
            pq <- length(setdiff(intersect(n1, n2n1[[p[1, j]]]),
                                 n2n1[[p[2, j]]])) > 0
            # Whether Q and R share an exclusive event
            qr <- length(setdiff(intersect(n1, n2n1[[p[2, j]]]),
                                 n2n1[[p[1, j]]])) > 0
            # How many events P, Q, and R share
            pqr <- length(intersect(intersect(n1, n2n1[[p[1, j]]]),
                                    n2n1[[p[2, j]]]))
            # If revelant, whether P and R share an exclusive event
            pr <- if(pq + qr + pqr < 2) 0 else
                (length(setdiff(intersect(n2n1[[p[1, j]]],
                                          n2n1[[p[2, j]]]), n1)) > 0)
            # Counts
            Ws <- c(pq * qr, c(pq, qr) * (pqr > 0), pqr > 1)
            Ts <- c(pr + pqr > 0, rep(pr | (pqr > 1), 2), pr | (pqr > 2))
            return(c(sum(Ws), sum(Ws * Ts)))
        })
        return(rowSums(wedgelist))
    }

injstr_transitivity <-
    function(
        bigraph, type = "global",
        vids = which(!V(bigraph)$type)
    ) {
        transitivity_an(
            bigraph = bigraph, type = type,
            wedgeFun = injstr_wedges, vids = vids)
    }

indstr_wedges <-
    function(bigraph, Q) {
        # Identify nodes of separation (exactly) 1 and 2 from Q
        n1 <- setdiff(neighborhood(bigraph, 1, Q)[[1]], Q)
        n2 <- setdiff(neighborhood(bigraph, 2, Q)[[1]], c(n1, Q)) # rm Q?
        # Require at least two nodes of separation 2 for a wedge
        if(length(n2) < 2) return(c(0, 0))
        # Identify secondary neighbors of primary neighbors P of Q (excluding P)
        n2n1 <- lapply(n2,
                       function(P) setdiff(neighborhood(bigraph, 1, P)[[1]], P))
        # Identify indexes of pairs (P, R) of nodes in n2
        p <- utils::combn(1:length(n2), 2)
        # Remove pairs (P, R) with no pairwise exclusive secondary neighbors
        p <- as.matrix(p[, sapply(1:dim(p)[2], function(j) {
            (0 < length(setdiff(intersect(n2n1[[p[1, j]]], n1),
                                n2n1[[p[2, j]]])) *
                 length(setdiff(intersect(n2n1[[p[2, j]]], n1),
                                n2n1[[p[1, j]]])))
        })], nr = 2)
        # Require at least two nodes of separation 2 for a wedge
        if(dim(p)[2] == 0) return(c(0, 0))
        # Identify which of these pairs share a neighbor not shared with Q
        cl <- sapply(1:dim(p)[2], function(j) {
            0 < length(setdiff(intersect(n2n1[[p[1, j]]], n2n1[[p[2, j]]]), n1))
        })
        # Return the counts
        return(c(length(cl), sum(cl)))
    }

indstr_transitivity <-
    function(
        bigraph, type = "global",
        vids = which(!V(bigraph)$type)
    ) {
        transitivity_an(
            bigraph = bigraph, type = type,
            wedgeFun = indstr_wedges, vids = vids)
    }

injact_wedges <-
    function(bigraph, Q) {
        # Identify nodes of separation (exactly) 1 and 2 from Q
        n1 <- setdiff(neighborhood(bigraph, 1, Q)[[1]], Q)
        n2 <- setdiff(neighborhood(bigraph, 2, Q)[[1]], c(n1, Q))
        # Require at least two nodes of separation 2 for a wedge
        if(length(n2) < 2) return(c(0, 0))
        # Identify pairs (P, R) of nodes in n2
        p <- utils::combn(n2, 2)
        # Identify which of these pairs form wedges & which of these are closed
        wedgelist <- sapply(1:dim(p)[2], function(j) {
            # Secondary neighbors of P and of R
            pn1 <- neighborhood(bigraph, 1, p[1:2, j])
            # Common neighbors of P and R
            tn1 <- do.call(intersect, pn1)
            # If only one secondary links either to Q then no wedges exist
            if(length(intersect(n1, unique(unlist(pn1)))) == 1) c(0, 0) else
                # Otherwise one wedge, closed iff Hall criterion is met
                c(1, as.numeric(hallCriterion(list(intersect(n1, pn1[[1]]),
                                                   intersect(n1, pn1[[2]]),
                                                   tn1))))
        })
        return(rowSums(wedgelist))
    }

injact_transitivity <-
    function(
        bigraph, type = "global",
        vids = which(!V(bigraph)$type)
    ) {
        transitivity_an(
            bigraph = bigraph, type = type,
            wedgeFun = injact_wedges, vids = vids)
    }

injequ_wedges <-
    function(bigraph, Q) {
        # Identify secondary neighbors of Q
        n1 <- setdiff(neighborhood(bigraph, 1, Q)[[1]], Q)
        # If there aren't at least two, return zeroes
        if(length(n1) < 2) return(c(0, 0))
        # Identify primary neighborhoods of secondary neighbors of Q
        n1n1 <- lapply(neighborhood(bigraph, 1, n1), setdiff, c(Q, n1))
        # Array the 2-paths centered at Q
        # (Note that these are indices of n1, not vertex ids)
        p <- utils::combn(1:length(n1), 2)
        # Across the pairs (X, Y) list the numbers of wedges and closed wedges
        wedgelist <- do.call(cbind, lapply(1:ncol(p), function(j) {
            # The first node X must have a nonempty neighborhood besides Q
            if(length(n1n1[[p[1, j]]]) == 0) return(c(0, 0))
            # Across all choices of P from the non-Q primary neighbors of X
            do.call(cbind, lapply(n1n1[[p[1, j]]], function(P) {
                # The second node Y must have a nonempty nbhd besides Q and P
                Rs <- setdiff(n1n1[[p[2, j]]], P)
                if(length(Rs) == 0) return(c(0, 0))
                # Which Rs produce 4-paths (P, X, Q, Y, R) that are closed?
                Rw <- which(sapply(Rs, function(R) {
                    length(setdiff(intersect(neighborhood(bigraph, 1, P)[[1]],
                                             neighborhood(bigraph, 1, R)[[1]]),
                                   n1[p[, j]])) > 0
                }))
                return(c(length(Rs), length(Rw)))
            }))
        }))
        rowSums(wedgelist)
    }

injequ_transitivity <-
    function(
        bigraph, type = "global",
        vids = which(!V(bigraph)$type)
    ) {
        transitivity_an(
            bigraph = bigraph, type = type,
            wedgeFun = injequ_wedges, vids = vids)
    }

project_transitivity <-
    function(
        bigraph, type = "global",
        vids = which(!V(bigraph)$type)
    ) {
        if(vcount(bigraph) == 0) {
            if(type == "global") {
                return(NaN)
            } else if(type == "local") {
                return(NULL)
            } else return(matrix(NA, nr = 0, nc = 2))
        }
        stopifnot(all(!V(bigraph)$type[vids]))
        graph <- actor_projection(bigraph)
        proj.vids <- which(which(!V(bigraph)$type) %in% vids)
        stopifnot(length(proj.vids) == length(vids))
        if(type == "global") {
            return(transitivity(graph, type = "global"))
        }
        C <- transitivity(graph, type = "local", vids = proj.vids)
        if(type == "local") return(C)
        C[is.na(C)] <- 0
        W <- choose(degree(graph)[proj.vids], 2)
        unname(cbind(W, W * C))
    }

opsahl_transitivity <-
    function(
        bigraph, type = "global",
        vids = which(!V(bigraph)$type)
    ) {
        transitivity_an(
            bigraph = bigraph, type = type,
            wedgeFun = injequ_wedges, vids = vids)
    }

excl_transitivity <-
    function(
        bigraph, type = "global",
        vids = which(!V(bigraph)$type)
    ) {
        transitivity_an(
            bigraph = bigraph, type = type,
            wedgeFun = indstr_wedges, vids = vids)
    }
