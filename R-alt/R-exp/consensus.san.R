#' Consensus in a signed affiliation network
#' 
#' In a signed affiliation network each actor-event tie may be positive (1),
#' negative (-1), or neutral (0). The consensus of actors tied to an event is
#' given by (C + D) / choose(n, 2), where n is the number of actors and C and D
#' are the number of pairs of actors who agree and disagree, respectively.
#' @param bigraph A signed affiliation network.
#' @param type Character; the type of statistic. Defaults to "global".
#' @param vids A subset of event node ids at which to evaluate the consensus.
#' @param sign.only Logical; whether only equal and opposite signs should be
#' tallied as agreements and disagreements. Defaults to `FALSE`.
#' @export

consensus.san <-
    function(
        bigraph, type = "global", vids = which(V(bigraph)$type),
        sign.only = FALSE, add.names = TRUE
    ) {
        if(length(vids) == 0) {
            return(NULL)
        }
        # Decide type of statistic; add events if necessary
        type <- match.arg(type, c("global", "local", "table"))
        if(type == "global" & length(vids) != event.count(bigraph)) {
            vids <- which(V(bigraph)$type)
        } else {
            # Check that nodes are events
            stopifnot(all(V(bigraph)$type[vids]))
        }
        # Tallies by event
        signs <- do.call(rbind, lapply(vids, function(vid) {
            # Cut into factors so that table doesn't get confused
            # (would prefer to be able to just define table bins)
            vec <- cut(E(bigraph)[inc(vid)]$sign, breaks = -2:1, labels = -1:1)
            table(vec)
            # Alternatively, add 2 and use tabulate()
            #tabulate(E(bigraph)[inc(vid)]$sign + 2) - 2
            # NEED TO BENCHMARK THESE TWO OPTIONS
        }))
        if(add.names) {
            rownames(signs) <- V(bigraph)$name[vids]
        }
        # Pairs in agreement
        agree <- if(sign.only) {
            rowSums(choose(signs[, c(1, 3)], 2))
        } else {
            rowSums(choose(signs, 2))
        }
        # Pairs in disagreement
        disagree <- if(sign.only) {
            signs[, 1] * signs[, 3]
        } else {
            signs[, 1] * (signs[, 2] + signs[, 3]) + signs[, 2] * signs[, 3]
        }
        # Return desired statistic
        if(type == "global") {
            C <- sum(agree)
            D <- sum(disagree)
            return((C - D) / (C + D))
        } else if(type == "local") {
            return((agree - disagree) / (agree + disagree))
        } else {
            return(cbind(agree, disagree))
        }
    }
