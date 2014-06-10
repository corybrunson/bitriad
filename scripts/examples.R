# Create graph objects for two-mode network data sets
mydir <- 'https://raw.githubusercontent.com/corybrunson/triadic/master/'
mytable <- function(data.file, ...) {
    require(RCurl)
    read.table(text = getURL(paste(mydir, 'data/', data.file, sep = '')), ...)
}
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
mysource <- function(src.file) source_https(paste(mydir, src.file, sep = ''))
myfn <- function(fn.file) {
    source_https(paste(mydir, 'functions/', fn.file, sep = ''))
}
mysource('functions/triadic.R') # unsupported URL scheme

# "Deep South" > "Social Cliques in Colored Society" > Fig. 11: Clique A
# Read data directly into bipartite incidence matrix
women <- graph.incidence(as.matrix(mycsv('DGG_Clique_A.csv', row.names = 1)))

# Davis Southern Women Club
# Read data into table first (since same numbers are used for different modes)
data <- mytable('Davis_southern_club_women-two_mode.txt', colClasses = 'numeric')
names <- mytable('Davis_southern_club_women-name.txt', colClasses = 'character')
davis <- graph.data.frame(data.frame(woman = names[data[, 1], 1],
                                     event = data[, 2]), directed = FALSE)
V(davis)$type <- !(substr(V(davis)$name, 1, 1) %in% LETTERS)

# Fischer Whig groups
# http://www.sscnet.ucla.edu/polisci/faculty/chwe/ps269/han.pdf
# http://kieranhealy.org/blog/archives/2013/06/09/
# using-metadata-to-find-paul-revere/
# https://github.com/kjhealy/revere
# Read data directly into bipartite incidence matrix
whigs <- graph.incidence(as.matrix(mycsv('PaulRevereAppD.csv', row.names = 1)))
# Remove non-group affiliations (Tea Party and London Enemies)
whigs <- delete.vertices(whigs,
                         which(V(whigs)$name %in% c('TeaParty',
                                                    'LondonEnemies')))

# Barnes-Burkett corporate board affiliations
# Read data directly into bipartite incidence matrix
board <- graph.incidence(as.matrix(mycsv('corporate_directors.csv',
                                            row.names = 1)[, 1:15]))

