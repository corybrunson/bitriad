mysrc <- function(src.file) source(paste(mydir, src.file, sep = ''))

# igraph library
library(igraph)

# Indexing scheme
mysrc('partition.bijections.R')

# Candidate clustering coefficients
mysrc('twomode.transitivity.R')

# Triad census functions
mysrc('twomode.triad.census.R')
mysrc('tc2ccs.R')
