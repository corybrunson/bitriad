#' Balance in a signed affiliation network
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
#' configurations, respectively. The balance of the network is given by the same
#' expression, taken over all configurations rather than those specific to a
#' pair of actors or events.
#' @param bigraph A signed affiliation network.
#' @param type Character; the type of statistic. Defaults to "global".
#' @param vids A vector of vertex ids of like type at which to evaluate the
#' balance.
#' @param sign.only Logical; whether only equal and opposite signs should be
#' tallied as agreements and disagreements. Defaults to `FALSE`.
#' @export

balance.san <-
    function(
        bigraph, type = "global", vids = NULL,
        sign.only = FALSE, use.names = TRUE
    ) {
        # Decide type of statistic; add events if necessary
        type <- match.arg(type, c("global", "actors", "events"))
        if(type == "global") {
            vids <- if(actor.count(bigraph) > event.count(bigraph)) {
                V(bigraph)[which(V(bigraph)$type)]
            } else {
                V(bigraph)[which(!V(bigraph)$type)]
            }
        }
        if(is.null(vids)) {
            vids <- if(type == "actors") {
                V(bigraph)[which(!V(bigraph)$type)]
            } else {
                V(bigraph)[which(V(bigraph)$type)]
            }
        }
        # Incidence matrices
        tie.inc <- get.incidence(bigraph)
        sgn.inc <- get.incidence(bigraph, attr = "sign")
        # Transpose if necessary
        if(type == "events") {
            tie.inc <- t(tie.inc)
            sgn.inc <- t(sgn.inc)
        }
        # Adjacency matrices
        tie.adj <- tie.inc %*% t(tie.inc)
        sgn.adj <- sgn.inc %*% t(sgn.inc)
        if(!sign.only) {
            # Accumulate 2 for each balance and -1 for each unbalance;
            # need to subtract 1 for each balance
            i <- 0 # number of cycles performed
            bal.inc <- sgn.inc * (sgn.inc == 1)
            bal.adj <- bal.inc %*% t(bal.inc)
            repeat {
                E(bigraph)$sign <- E(bigraph)$sign + 1
                E(bigraph)$sign[E(bigraph)$sign == 2] <- -1
                sgn.inc <- get.incidence(bigraph, attr = "sign")
                if(type == "events") {
                    sgn.inc <- t(sgn.inc)
                }
                i <- i + 1
                if(i >= 3) break # stop at third cycle (back to normal)
                sgn.adj <- sgn.adj + sgn.inc %*% t(sgn.inc) # add 1st & 2nd
                bal.inc <- sgn.inc * (sgn.inc == 1)
                bal.adj <- bal.adj + bal.inc %*% t(bal.inc)
            }
            # Subtract 1 for each balance
            sgn.adj <- sgn.adj - bal.adj
        }
        # Return desired statistic
        if(type == "global") {
            C <- sum(sgn.adj)
            CD <- sum(tie.adj)
            return(unname((2 * C - CD) / CD))
        } else {
            if(!use.names) {
                sgn.adj <- unname(sgn.adj)
                tie.adj <- unname(sgn.adj)
            }
            return((2 * sgn.adj - tie.adj) / tie.adj)
        }
    }
