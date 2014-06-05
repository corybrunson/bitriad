# Pairwise weight–resolved exclusive clustering
# tc must be a matrix with rows indexed as partition.position
tc2Cexij <- function(tc, i, j, num.denom = FALSE) tc2cc(tc,
  function(L, w) 3 * ((L[2] >= i) & (L[3] >= j)),
  function(L, w) ((L[2] >= i) & (L[3] < j)), num.denom)

