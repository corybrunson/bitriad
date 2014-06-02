# Triad weight–resolved exclusive clustering
# tc must be a matrix with rows indexed as partition.position
tc2Cexw <- function(tc, ww, num.denom = FALSE) tc2cc(tc,
  function(L, w) 3 * ((L[3] > 0) & (w == ww)),
  function(L, w) ((L[2] > 0) & (L[3] == 0) & (w == ww)), num.denom)

