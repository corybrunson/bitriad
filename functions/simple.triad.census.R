simple.triad.census <- function(graph, rcnames = FALSE) {
  tc <- triad.census(as.directed(graph))
  if(is.nan(tc[1])) tc[1] <- choose(vcount(graph), 3) - sum(tc, na.rm = TRUE)
  stc <- tc[c(1, 3, 11, 16)]
  if(rcnames) names(stc) <- 0:3
  return(stc)
}

