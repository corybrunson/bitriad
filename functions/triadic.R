# igraph library
library(igraph)

# Source directory
dir <- 'https://raw.githubusercontent.com/corybrunson/triadic/master/functions/'
source.dir <- function(file) source(paste(dir, file, sep = ''))

# Indexing scheme
source.dir('partition.bijections.R')

# Candidate clustering coefficients
source.dir('assortative.transitivity.R')
source.dir('bipartite.transitivity.R')
source.dir('sdr.criterion.R')
source.dir('inclusive.transitivity.R')
source.dir('exclusive.transitivity.R')

# Triad census functions
source.dir('simple.triad.census.R')
source.dir('twomode.triad.census.R')
source.dir('tc2ccs.R')
