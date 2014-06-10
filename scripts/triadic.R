# Source directory
mydir <- 'https://raw.githubusercontent.com/corybrunson/triadic/master/'
mycsv <- function(data.file, ...) {
  require(RCurl)
  read.csv(text = getURL(paste(mydir, 'data/', data.file, sep = '')), ...)
}
source_https <- function(url, ...) {
  require(RCurl)
  # parse and evaluate each .R script
  sapply(c(url, ...), function(u) {
    eval(parse(text = getURL(u, followlocation = TRUE,
                             cainfo = system.file("CurlSSL", "cacert.pem",
                                                  package = "RCurl"))),
         envir = .GlobalEnv)
  })
}
mysrc <- function(src.file) source_https(paste(mydir, src.file, sep = ''))
myfn <- function(fn.file) {
  source_https(paste(mydir, 'functions/', fn.file, sep = ''))
}

# igraph library
library(igraph)

# Indexing scheme
myfn('partition.bijections.R')

# Candidate clustering coefficients
myfn('twomode.transitivity.R')

# Triad census functions
myfn('twomode.triad.census.R')
myfn('tc2ccs.R')
