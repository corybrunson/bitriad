# Create graph objects for two-mode network data sets
source('triadic.R')

# Davis Southern Women Club
# Read data into table first (since same numbers are used for different modes)
data <- read.table('Davis_southern_club_women-two_mode.txt',
                   colClasses = 'numeric')
names <- read.table('Davis_southern_club_women-name.txt',
                          colClasses = 'character')
davis <- graph.data.frame(data.frame(woman = names[data[, 1], 1],
                                     event = data[, 2]), directed = FALSE)
V(davis)$type <- substr(V(davis)$name, 1, 1) %in% LETTERS

# Fischer Whig groups
# http://www.sscnet.ucla.edu/polisci/faculty/chwe/ps269/han.pdf
# http://kieranhealy.org/blog/archives/2013/06/09/using-metadata-to-find-paul-revere/
# https://github.com/kjhealy/revere
# Read data directly into bipartite incidence matrix
whigs <- graph.incidence(as.matrix(read.csv('PaulRevereAppD.csv',
                                            row.names = 1)))
V(whigs)$type <- !V(whigs)$type
# Remove non-group affiliations (Tea Party and London Enemies)
whigs <- delete.vertices(whigs,
                         which(V(whigs)$name %in% c('TeaParty',
                                                    'LondonEnemies')))

# Barnes-Burkett corporate board affiliations
# Read data directly into bipartite incidence matrix
board <- graph.incidence(as.matrix(read.csv('corporate_directors.csv',
                                            row.names = 1)[, 1:15]))
V(board)$type <- !V(board)$type

# Local clustering coefficients
# Davis network
davis.local <- data.frame(
  'C' = transitivity(bipartite.projection(davis)[[2]], type = 'local'),
  'C.*' = bipartite.transitivity(davis, type = 'local'),
  'C.N' = inclusive.transitivity(davis, type = 'local'),
  'C.X' = exclusive.transitivity(davis, type = 'local'))
row.names(davis.local) <- V(davis)$name[V(davis)$type]
# Whigs network
whigs.local <- data.frame(
  'C' = transitivity(bipartite.projection(whigs)[[2]], type = 'local'),
  'C.*' = bipartite.transitivity(whigs, type = 'local'),
  'C.N' = inclusive.transitivity(whigs, type = 'local'),
  'C.X' = exclusive.transitivity(whigs, type = 'local'))
row.names(whigs.local) <- V(whigs)$name[V(whigs)$type]
# Board network
board.local <- data.frame(
  'C' = transitivity(bipartite.projection(board)[[2]], type = 'local'),
  'C.*' = bipartite.transitivity(board, type = 'local'),
  'C.N' = inclusive.transitivity(board, type = 'local'),
  'C.X' = exclusive.transitivity(board, type = 'local'))
row.names(board.local) <- V(board)$name[V(board)$type]

save(davis, whigs, board,
     davis.local, whigs.local, board.local,
     file = 'examples.RData')

rm(list = ls())
