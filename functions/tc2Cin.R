# Global inclusive clustering coefficient
# (existence of not necessarily induced 4-paths and 6-cycles)
# tc must be a matrix with rows indexed as partition.position
tc2Cin <- function(tc, num.denom = FALSE) tc2cc(tc,
  function(L, w) 3 * (length(which(L > 0)) + w > 2),
  function(L, w) (L[2] > 0 & L[3] == 0 & w == 0) +
    2 * (L[1] > 0 & L[2] == 0 & w == 1) +
    3 * (L[1] == 0 & w == 2),
  num.denom = num.denom)

