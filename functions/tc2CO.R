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

