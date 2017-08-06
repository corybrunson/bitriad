#' Triad tallies
#'
#' These functions are called by the full triad census to handle triads of
#' different types using the projection onto actor nodes. The name of each
#' function indicates the number of edges that appear among the three actors of
#' the triad in the projection. (Zero-edge triads do not need to be tallied;
#' the total number of triads is easily calculated, and the difference between
#' this number and the total number of triads with edges gives the number of
#' triads without.)
#' 
#' @name triad_tallies
#' @param graph A one-mode network
#' @param bigraph The ambient affiliation network from which graph is projected
connectedTriples <-
  function(
    bigraph,
    # Construct the one-mode projection if it's not already prepared
    graph = actor_projection(bigraph, name = 'id')
  ) {
    trips <- do.call(rbind, lapply(1:vcount(graph), function(i) {
      nbhd <- neighborhood(graph, 1, i)[[1]]
      # Skip nodes with not enough neighbors
      if(length(nbhd) < 2) return(NULL)
      # horizontal array of pairs of neighbors of i
      v <- utils::combn(setdiff(nbhd, i), 2)
      # vector of triad weights
      w <- sapply(1:dim(v)[2], function(j) {
        shareWeight(bigraph, V(graph)$name[c(i, v[, j])])
      })
      # horizontal array of sorted triples of edge weights
      ew <- sapply(1:dim(v)[2], function(j) {
        sort(c(edgeWeight(graph, c(i, v[1, j])),
               edgeWeight(graph, c(i, v[2, j])),
               edgeWeight(graph, c(v[1, j], v[2, j]))),
             decreasing = TRUE)
      })
      # vertical array of pair and triad weights
      return(data.frame(x = ew[1, ] - w, y = ew[2, ] - w,
                        z = ew[3, ] - w, w = w))
    }))
    return(stats::aggregate(
      n ~ x * y * z * w, FUN = sum,
      data = cbind(trips,
                   n = 1 - (trips$z + trips$w > 0) * 2 / 3)
    ))
  }

#' @rdname triad_tallies
oneTiedTriads <-
  function(graph) {
    # Create a data frame of weights and number of nonadjacent nodes
    counts <- data.frame(
      x = E(graph)$weight,
      n = vcount(graph) - as.numeric(sapply(1:ecount(graph), function(i) {
        length(unique(unlist(neighborhood(graph, order = 1,
                                          ends(graph, i)))))
      }))
    )
    # Return the aggregated data frame
    return(stats::aggregate(n ~ x, data = counts, FUN = sum))
  }

#' @rdname triad_tallies
twoTiedTriads <-
  function(graph) {
    # List of open wedges (shortest paths of length 2) up to reversal
    p2 <- do.call(cbind, lapply(
      V(graph)[1:(vcount(graph) - 1)], function(v) {
        d2 <- as.numeric(V(graph)[
          which(distances(graph, v, (v + 1):vcount(graph),
                          weights = NA) == 2) + v
          ])
        gasp <- all_shortest_paths(graph, v, d2, weights = NA)[[1]]
        do.call(cbind, gasp[sapply(gasp, length) == 3])
      }
    ))
    # Horizontal array of sorted edge weight pairs
    if(is.null(p2)) return(NULL) else  wedges <- sapply(
      1:dim(p2)[2],
      function(j) sort(c(edgeWeight(graph, c(p2[1, j], p2[2, j])),
                         edgeWeight(graph, c(p2[2, j], p2[3, j]))),
                       decreasing = TRUE))
    # Make wedges into a data frame
    wedges <- data.frame(x = wedges[1, ], y = wedges[2, ], n = 1)
    # Return the aggregated data frame
    return(stats::aggregate(
      n ~ x * y, FUN = sum,
      data = cbind(wedges, n = rep(1, n = dim(wedges)[1]))
    ))
  }

#' @rdname triad_tallies
threeTiedTriads <-
  function(
    bigraph,
    # Construct the one-mode projection if it's not already prepared
    graph = actor_projection(bigraph, name = 'id')
  ) {
    # Triangles are 3-cliques in the one-mode projection
    t <- do.call(cbind, cliques(graph, min = 3, max = 3))
    # If there are no triangles then return an empty list
    if(is.null(t)) return(NULL)
    # Vector of triad weights
    w <- sapply(1:dim(t)[2], function(j) {
      shareWeight(bigraph, V(graph)$name[c(t[1, j], t[2, j], t[3, j])])
    })
    # Horizontal array of sorted triples of edge weights
    ew <- sapply(1:dim(t)[2], function(j) {
      sort(c(edgeWeight(graph, c(t[1, j], t[2, j])),
             edgeWeight(graph, c(t[2, j], t[3, j])),
             edgeWeight(graph, c(t[1, j], t[3, j]))),
           decreasing = TRUE)
    })
    tris <- data.frame(x = ew[1, ] - w, y = ew[2, ] - w,
                       z = ew[3, ] - w, w = w)
    stats::aggregate(n ~ x * y * z * w, data = cbind(tris, n = 1),
                     FUN = sum)
  }
