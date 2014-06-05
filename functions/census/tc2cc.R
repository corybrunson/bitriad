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

