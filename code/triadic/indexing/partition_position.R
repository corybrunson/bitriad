# Rev-lex position of a given k-part partition
partition.position <- function(lambda) {
  combination.position(partition.combination(lambda))
}

