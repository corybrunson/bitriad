simple.triad.census <- function(graph) {
  tc <- triad.census(as.directed(graph))
  if(is.nan(tc[1])) tc[1] <- choose(vcount(graph), 3) - sum(tc, na.rm = TRUE)
  tc[c(1, 3, 11, 16)]}

