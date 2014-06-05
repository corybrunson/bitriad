# k-part partition at a given rev-lex position
position.partition <- function(k, i) {
  combination.partition(position.combination(k, i))
}

