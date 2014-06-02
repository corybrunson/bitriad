# Global exclusive clustering coefficient
# (existence of induced 4-paths and 6-cycles)
# (agrees with exclusive.transitivity)
# tc must be a matrix with rows indexed as partition.position
tc2Cex <- function(tc, num.denom = FALSE) tc2cc(tc,
  function(L, w) 3 * (L[3] > 0),
  function(L, w) ((L[2] > 0) & (L[3] == 0)), num.denom)

