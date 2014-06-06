# Create graph objects for two-mode network data sets
library(igraph)
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
source_https('https://raw.githubusercontent.com/corybrunson/triadic/master/functions/triadic.R')

# "Deep South" > "Social Cliques in Colored Society" > Fig. 11: Clique A
# Read data directly into bipartite incidence matrix
women <- graph.incidence(as.matrix(read.csv('data/tmtc/DGG_Clique_A.csv',
                                            row.names = 1)))
V(women)$type <- !V(women)$type

# Davis Southern Women Club
# Read data into table first (since same numbers are used for different modes)
data <- read.table('data/tmtc/Davis_southern_club_women-two_mode.txt',
                   colClasses = 'numeric')
names <- read.table('data/tmtc/Davis_southern_club_women-name.txt',
                    colClasses = 'character')
davis <- graph.data.frame(data.frame(woman = names[data[, 1], 1],
                                     event = data[, 2]), directed = FALSE)
V(davis)$type <- substr(V(davis)$name, 1, 1) %in% LETTERS

# Fischer Whig groups
# http://www.sscnet.ucla.edu/polisci/faculty/chwe/ps269/han.pdf
# http://kieranhealy.org/blog/archives/2013/06/09/
# using-metadata-to-find-paul-revere/
# https://github.com/kjhealy/revere
# Read data directly into bipartite incidence matrix
whigs <- graph.incidence(as.matrix(read.csv('data/tmtc/PaulRevereAppD.csv',
                                            row.names = 1)))
V(whigs)$type <- !V(whigs)$type
# Remove non-group affiliations (Tea Party and London Enemies)
whigs <- delete.vertices(whigs,
                         which(V(whigs)$name %in% c('TeaParty',
                                                    'LondonEnemies')))

# Barnes-Burkett corporate board affiliations
# Read data directly into bipartite incidence matrix
board <- graph.incidence(as.matrix(read.csv('data/tmtc/corporate_directors.csv',
                                            row.names = 1)[, 1:15]))
V(board)$type <- !V(board)$type

