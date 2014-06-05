# Biject between positions and combinations

# k-combination at a given rev-lex position
position.combination <- function(k, i) {
  vec <- c()
  N <- i
  for(j in k:1) {
    c <- j - 1
    while(choose(c + 1, j) <= N) c <- c + 1
    vec <- c(vec, c)
    N <- N - choose(c, j)
  }
  return(vec)
}

# Rev-lex position of a given k-combination of [0 : (n - 1)], k = length(vec)
combination.position <- function(vec) {
  stopifnot(!is.unsorted(rev(vec), strictly = TRUE))
  sum(choose(rev(vec), 1:length(vec)))
}

# Biject between combinations and partitions

# k-part partition corresponding to a given k-combination of [0 : (n - 1)]
combination.partition <- function(vec) vec - (length(vec) - 1):0

# k-combination of [0 : (n - 1)] corresponding to a given k-part partition
partition.combination <- function(lambda) lambda + (length(lambda) - 1):0

# Compose the bijections to biject between positions and partitions

# k-part partition at a given rev-lex position
position.partition <- function(k, i) {
  combination.partition(position.combination(k, i))
}

# Rev-lex position of a given k-part partition
partition.position <- function(lambda) {
  combination.position(partition.combination(lambda))
}

