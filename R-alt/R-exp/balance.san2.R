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
#' @param vids A subset of event node ids at which to evaluate the balance
#' @param sign.only Logical; whether only equal and opposite signs should be
#' tallied as agreements and disagreements. Defaults to `FALSE`.
#' @export

balance.san2 <-
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
        # 2-row matrix of pairs from `vids`
        pv <- combn(as.numeric(vids), 2)
        # Apply `balance.pair` across these pairs
        pb <- apply(pv, 2, function(x) {
            balance.pair(bigraph = bigraph, v1 = x[1], v2 = x[2],
                         sign.only = sign.only)
        })
        # Calculate local balance, or return if global
        if(type == "global") {
            C <- sum(pb[1, ])
            D <- sum(pb[2, ])
            return((C - D) / (C + D))
        }
        b <- (pb[1, ] - pb[2, ]) / colSums(pb)
        # Put into matrix form (MAKE IDENTITY MATRIX)
        balance.mat <- matrix(0, nrow = length(vids), ncol = length(vids))
        pn <- combn(1:length(vids), 2)
        for(j in 1:ncol(pn)) {
            balance.mat[pn[1, j], pn[2, j]] <- b[j]
        }
        balance.mat <- balance.mat + t(balance.mat)
        if(use.names) {
            rownames(balance.mat) <- V(bigraph)$name[vids]
            colnames(balance.mat) <- V(bigraph)$name[vids]
        }
        # Return matrix
        return(balance.mat)
    }
