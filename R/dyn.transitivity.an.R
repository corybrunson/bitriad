#' Dynamic triadic closure of an affiliation network
#'
#' Given an affiliation network with time-stamped events, compute the proportion
#' of centered triples at which an open wedge exists at some time that is
#' closed at a later time.
#' @param bigraph An affiliation network with time-stamped events.
#' @param memory Numeric; a duration of time after which events are forgotten.
#' @param type Character; whether to compute the global or local statistic, or
#' to return a 2-column matrix of wedge counts (defaults to "global").
#' @examples
#' data(women.group)
#' dyn.transitivity.an(women.group)
#' cbind(
#'     transitivity(actor.projection(women.group), type = "local"),
#'     opsahl.transitivity(women.group, type = "local"),
#'     excl.transitivity(women.group, type = "local"),
#'     dyn.transitivity.an(women.group, type = "local")
#' )
#' @export

dyn.transitivity.an <-
    function(graph, memory = Inf, type = "global") {

        if(vcount(graph) == 0) {
            # Empty resulting data frame
            wedges.dat <- data.frame(
                wedges = c(),
                closed = c()
            )
            # Tailor the output to global, local, or raw
            return(
                if(type == "global") {
                    sum(wedges.dat$closed, na.rm = TRUE) /
                        sum(wedges.dat$wedges, na.rm = TRUE)
                } else if(type == "local") {
                    wedges.dat$closed / wedges.dat$wedges
                } else wedges.dat
            )
        }
        # HOW TO SKIP AHEAD WITHIN A FUNCTION?

        if(!("name" %in% list.vertex.attributes(graph))) {
            stop('Actors need names')
        }

        # Make sure events increase with time, and put actors before events
        perm <- order(c(
            which(!V(graph)$type),
            which(V(graph)$type)[order(V(graph)$time[which(V(graph)$type)])]
        ))
        graph <- permute.vertices(graph, perm)

        # Actor names
        actors <- V(graph)$name[which(!V(graph)$type)]

        # All values of time, in order
        span <- sort(unique(V(graph)$time))

        # An empty array of wedges with closure counts (to be incremented)
        wedges <- matrix(NA, nr = 0, nc = 4)

        for(s in span[-1]) {

            # Remove events after time t or before time t - memory
            # from graph to get graph1
            graph1 <- delete.vertices(graph, which(
                V(graph)$type & (V(graph)$time > s | V(graph)$time < s - memory)
            ))

            # Remove events at time t from graph1 to get graph0
            graph0 <- delete.vertices(graph1, which(
                V(graph1)$type & (V(graph1)$time == s)
            ))

            # Remove any actors who did not attend any events in graph0
            # from both graph0 and graph1
            missing.actors <- which(
                !V(graph0)$type & (degree(graph0) == 0)
            )
            graph0 <- delete.vertices(graph0, missing.actors)
            # graph0 is the cumulative graph from s - memory to s, exclusive
            graph1 <- delete.vertices(graph1, missing.actors)
            # graph1 includes any new events at time s (but no new actors)

            # Check that the actors of graph0 and graph1 agree
            stopifnot(all(V(graph0)$name[!V(graph0)$type] ==
                              V(graph1)$name[!V(graph1)$type]))

            # Projections
            proj0 <- actor.projection(graph0)
            proj1 <- actor.projection(graph1)
            stopifnot(all(V(proj0)$name == V(proj1)$name))

            # Local transitivities of proj0
            tr0 <- transitivity(proj0, type = "local")

            # Exclude actors with undefined or complete transitivity
            wh.tr0 <- which((!is.na(tr0)) & (tr0 != 1))
            if(length(wh.tr0) == 0) next

            # Survey the wedges at these actors
            wedges01 <- do.call(rbind, lapply(wh.tr0, function(v) {
                # Actor's neighbors
                nbhd <- sort(
                    setdiff(neighborhood(proj0, order = 1, nodes = v)[[1]], v)
                )
                # Pairs of neighbors
                pairs <- t(matrix(combn(nbhd, 2), nr = 2))
                # Those that are not connected form wedges...
                pairs <- matrix(pairs[-which(apply(pairs, 1, function(x) {
                    are.connected(proj0, v1 = x[1], v2 = x[2])
                })), ], nc = 2)
                if(nrow(pairs) == 0) return(matrix(NA, nc = 4, nr = 0))
                # ...with v at the corner...
                pairs <- cbind(pairs[, 1], unname(v), pairs[, 2])
                # ...that are closed if the end nodes are tied in proj1
                pairs <- cbind(pairs, apply(pairs, 1, function(x) {
                    are.connected(proj1, v1 = x[1], v2 = x[3])
                }))
            }))
            if(nrow(wedges01) == 0) next

            # Convert IDs in wedges01 from those of proj0 to those of graph0
            wedges01[, 1:3] <-
                as.numeric(V(graph)[V(proj0)$name[wedges01[, 1:3]]])

            # Aggregate these with the existing survey
            wedges <- rbind(wedges, wedges01)
            wedges <- as.matrix(unname(aggregate(
                wedges[, 4],
                by = list(wedges[, 1], wedges[, 2], wedges[, 3]),
                FUN = max  # use `sum` to count closures
            )))

        }

        # Count the number of wedges and the number that close for each actor
        wedges.dat <- if(nrow(wedges) == 0) {
            data.frame(
                wedges = rep(0, length(actors)),
                closed = rep(0, length(actors))
            )
        } else {
            data.frame(
                wedges = tabulate(wedges[, 2], nbins = length(actors)),
                closed = sapply(1:length(actors), function(v) {
                    sum(wedges[which(wedges[, 2] == v), 4])
                })
            )
        }
        rownames(wedges.dat) <- actors

        # Tailor the output to global, local, or raw
        if(type == "global") {
            sum(wedges.dat$closed, na.rm = TRUE) /
                sum(wedges.dat$wedges, na.rm = TRUE)
        } else if(type == "local") {
            wedges.dat$closed / wedges.dat$wedges
        } else wedges.dat

    }
