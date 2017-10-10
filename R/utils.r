
edgeWeight <- function(graph, vp) {
  id <- get.edge.ids(graph, vp)
  if(id == 0) 0 else E(graph)$weight[id]
}

shareWeight <- function(bigraph, vids) {
  length(Reduce(intersect, neighborhood(bigraph, 1, as.numeric(vids))))
}

census_scheme <- function(census, scheme) {
  cdim <- dim(census)
  if (!is.null(scheme)) {
    scheme <- match.arg(scheme, c("full",
                                  "binary", "structural",
                                  "difference", "uniformity",
                                  "simple"))
    if (scheme == "structural") scheme <- "binary"
    if (scheme == "uniformity") scheme <- "difference"
    if (scheme == "difference" & any(cdim != c(8, 2))) {
      warning("A difference census must be formatted as a 8-by-2 matrix; ",
              "the input census will be treated as a full census.")
      scheme <- "full"
    }
    if (scheme == "binary" & any(cdim != c(4, 2))) {
      warning("A binary census must be formatted as a 4-by-2 matrix; ",
              "the input census will be treated as a full census.")
      scheme <- "full"
    }
    if (scheme == "simple" & any(cdim != c(4, 1))) {
      warning("A simple census must be formatted ",
              "as a 4-by-1 matrix or as a length-4 vector; ",
              "the input census will be treated as a full census.")
      scheme <- "full"
    }
  } else {
    scheme <- if (all(cdim == c(8, 2))) {
      "difference"
    } else  if (all(cdim == c(4, 1))) {
      "binary"
    } else  if (all(cdim == c(4, 1))) {
      "simple"
    } else {
      "full"
    }
  }
  scheme
}

# alcove, wedge, maps, and congruence codes associated with named measures
measure_codes <- list(
  classical = c(0, 0, 0, 2),
  projection = c(0, 0, 0, 2),
  watts_strogatz = c(0, 0, 0, 2),
  twomode = c(0, 0, 1, 0),
  opsahl = c(0, 0, 1, 0),
  unconnected = c(0, 0, 2, 0),
  liebig_rao_0 = c(0, 0, 2, 0),
  completely_connected = c(3, 2, 2, 0),
  liebig_rao_3 = c(3, 2, 2, 0),
  exclusive = c(0, 0, 2, 1)
)

# compress a wedgelist into a desired statistic
wedgeReturn <- function(wedgelist, type, add.names) {
  
  # global
  if (type == "global") {
    return(sum(wedgelist[, 2]) / sum(wedgelist[, 1]))
  }
  # local
  if (type == "local") {
    return(as.vector(wedgelist[, 2] / wedgelist[, 1]))
  }
  # otherwise
  #if (add.names) {
  #  rownames(wedgelist) <- V(graph)$name[vids]
  #  colnames(wedgelist) <- c("Wedges", "Closed")
  #}
  wedgelist
}
