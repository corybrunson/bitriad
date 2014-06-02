# Rev-lex position of a given k-combination of [0 : (n - 1)], k = length(vec)
combination.position <- function(vec) {
  stopifnot(!is.unsorted(rev(vec), strictly = TRUE))
  sum(choose(rev(vec), 1:length(vec)))
}

