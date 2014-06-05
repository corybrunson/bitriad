# Hall's criterion for the existence of a system of distinct representatives
sdr.criterion <- function(lst) all(sapply(0:(2 ^ length(lst) - 1), function(i) {
  w <- which(intToBits(i) == 1)
  length(unique(unlist(lst[w]))) >= length(w)
}))

