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

