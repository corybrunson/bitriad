# Classical clustering coefficient on the one-mode projection
# tc must be a matrix with rows indexed as partition.position
tc2C <- function(tc, num.denom = FALSE) tc2cc(tc,
  function(L, w) 3 * ((L[3] > 0) | (w > 0)),
  function(L, w) ((L[2] > 0) & (L[3] == 0) & (w == 0)), num.denom = num.denom)

