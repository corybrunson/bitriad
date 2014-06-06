# Derive clustering coefficients from two-mode triad census
tc2cc <- function(tc, S.fn, F.fn, num.denom = FALSE) {
  if(dim(tc)[1] * dim(tc)[2] == 0) return(NA)
  S.c <- sum(sapply(1:dim(tc)[2], function(j) sapply(1:dim(tc)[1], function(i) {
    if(tc[i, j] == 0) 0 else
    S.fn(position.partition(3, i - 1), j - 1) * tc[i, j]})))
  F.c <- sum(sapply(1:dim(tc)[2], function(j) sapply(1:dim(tc)[1], function(i) {
    if(tc[i, j] == 0) 0 else
    F.fn(position.partition(3, i - 1), j - 1) * tc[i, j]})))
  return(if(num.denom) c(S.c, S.c + F.c) else S.c / (S.c + F.c))}

# Classical clustering coefficient on the one-mode projection
# tc must be a matrix with rows indexed as partition.position
tc2C <- function(tc, num.denom = FALSE) tc2cc(tc,
  function(L, w) 3 * ((L[3] > 0) | (w > 0)),
  function(L, w) ((L[2] > 0) & (L[3] == 0) & (w == 0)), num.denom = num.denom)

# Global Opsahl clustering coefficient
# (agrees with bipartite.transitivity)
# tc must be a matrix with rows indexed as partition.position
tc2CO <- function(tc, num.denom = FALSE) tc2cc(tc,
  function(L, w) (L[1] * L[2] * (L[3] + w > 0) + L[1] * L[3] + L[2] * L[3] +
    L[1] * w * (L[2] > 0 | w > 1) + L[1] * w * (L[3] > 0 | w > 1) +
    L[2] * w + L[2] * w * (L[3] > 0 | w > 1) +
    2 * L[3] * w +
    2 * choose(w, 2) * max(3 * (w > 2), length(which(L > 0)))),
  function(L, w) (L[1] * L[2] * (L[3] + w == 0) +
    L[1] * (L[2] == 0 & w == 1) + L[1] * (L[3] == 0 & w == 1) +
    L[2] * (L[3] == 0 & w == 1) +
    2 * choose(w, 2) * min(3 * (w == 2), length(which(L == 0)))),
    num.denom = num.denom)

# Global inclusive clustering coefficient
# (existence of not necessarily induced 4-paths and 6-cycles)
# tc must be a matrix with rows indexed as partition.position
tc2Cin <- function(tc, num.denom = FALSE) tc2cc(tc,
  function(L, w) 3 * (length(which(L > 0)) + w > 2),
  function(L, w) (L[2] > 0 & L[3] == 0 & w == 0) +
    2 * (L[1] > 0 & L[2] == 0 & w == 1) +
    3 * (L[1] == 0 & w == 2),
  num.denom = num.denom)

# Global exclusive clustering coefficient
# (existence of induced 4-paths and 6-cycles)
# (agrees with exclusive.transitivity)
# tc must be a matrix with rows indexed as partition.position
tc2Cex <- function(tc, num.denom = FALSE) tc2cc(tc,
  function(L, w) 3 * (L[3] > 0),
  function(L, w) ((L[2] > 0) & (L[3] == 0)), num.denom)

# Pairwise weight–resolved exclusive clustering
# tc must be a matrix with rows indexed as partition.position
tc2Cexij <- function(tc, i, j, num.denom = FALSE) tc2cc(tc,
  function(L, w) 3 * ((L[2] >= i) & (L[3] >= j)),
  function(L, w) ((L[2] >= i) & (L[3] < j)), num.denom)

# Triad weight–resolved exclusive clustering
# tc must be a matrix with rows indexed as partition.position
tc2Cexw <- function(tc, ww, num.denom = FALSE) tc2cc(tc,
  function(L, w) 3 * ((L[3] > 0) & (w == ww)),
  function(L, w) ((L[2] > 0) & (L[3] == 0) & (w == ww)), num.denom)

