#' Balance of a pair
#' 
#' In a signed affiliation network each actor-event tie may be positive (1),
#' negative (-1), or neutral (0). A configuration of two actors both tied to
#' two events is balanced if the actors' pairs of opinions to the events are
#' similar. For example, if actors A and B agree on events X and Y, or if they
#' disagree on both X and Y, then A, B, X, and Y are balanced. If A and B agree
#' on X but disagree on Y then the configuration is not balanced. The balance
#' of two actors or events is given by (C + D) / choose(n, 2), where n is the
#' number of events (respectively, actors) tied to both and C and D are the
#' number of pairs of these actors who forrm balanced and imbalanced
#' configurations, respectively.
#' @param bigraph A signed affiliation network.
#' @param v1 A vertex id.
#' @param v2 A vertex id of the same type as `v1`.
#' @param sign.only Logical; whether only equal and opposite signs should be
#' tallied as agreements and disagreements. Defaults to `FALSE`.
#' @export

balance.pair <-
    function(bigraph, v1, v2, sign.only = FALSE) {
        # Check that v1 and v2 are of the same type
        stopifnot(V(bigraph)$type[v1] == V(bigraph)$type[v2])
        # Intersect the neighborhoods of v1 and v2
        n <- do.call(intersect, neighborhood(bigraph, 1, c(v1, v2)))
        # Edge ids in pairs in order of n
        e <- get.edge.ids(bigraph, rbind(v1, n, v2, n))
        # Edge signs
        s <- E(bigraph)$sign[e]
        # As a 2-column matrix
        S <- matrix(s, ncol = 2, byrow = TRUE)
        # Return agree/disagree pair using prescribed convention
        if(sign.only) {
            p <- apply(S, 1, prod)
            return(c(agree = length(which(p == 1)),
                     disagree = length(which(p == -1))))
        } else {
            p <- apply(S, 1, function(x) x[1] == x[2])
            return(c(agree = length(which(p)), disagree = length(which(!p))))
        }
    }
