# approximate clustering corrected for assortativity (Soffer–Vasquez 2005)
assortative.transitivity <- function(graph, vids = V(graph), type = 'global') {
  k <- degree(graph)
  t <- transitivity(graph, vids = vids, type = 'local') * choose(k[vids], 2)
  Omega <- sapply(neighborhood(graph, 1, nodes = vids), function(vec) {
    k0 <- length(vec) - 1
    if(k0 < 1) NaN else
    floor(sum(sapply(vec[-1], function(v) min(k0, k[v])) - 1) / 2)
  })
  if(type == 'local') t / Omega else
  if(type == 'global') sum(t, na.rm = TRUE) / sum(Omega, na.rm = TRUE) else
  return(list(stratified = data.frame(Omega = Omega, t = t),
              global = sum(t, na.rm = TRUE) / sum(Omega, na.rm = TRUE)))
}

