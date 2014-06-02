# k-combination of [0 : (n - 1)] corresponding to a given k-part partition
partition.combination <- function(lambda) lambda + (length(lambda) - 1):0

