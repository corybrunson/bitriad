Triadic analyses of two-mode networks
=====================================

The paper "Triadic analysis for two-mode networks" makes a case for adopting a coherent batch of triad-centric tools for the study of two-mode, usually affiliation, networks. This R Markdown file will apply these tools to the study of several manageably-sized real-world affiliation networks, in hopes of giving the reader a feel for what they mean, how they can be used, and what can be learned from them.

We use the "igraph" package, which provides the class of graphs and the basic suite of tools we build upon. We'll also read data and functions from the github account corybrunson; the function 'source_https' is taken from [tonybreyal][].

[tonybreyal] http://tonybreyal.wordpress.com/2011/11/24/source_https-sourcing-an-r-script-from-github/ "tonybreyal"


```r
library(igraph)
library(RCurl)
```

```
## Loading required package: bitops
```

```r
mydir <- 'https://raw.githubusercontent.com/corybrunson/triadic/master/'
mycsv <- function(data.file, ...) {
  read.csv(text = getURL(paste(mydir, 'data/', data.file, sep = '')), ...)
}
source_https <- function(url, ...) {
  # load package
  require(RCurl)
 
  # parse and evaluate each .R script
  sapply(c(url, ...), function(u) {
    eval(parse(text = getURL(u, followlocation = TRUE,
                             cainfo = system.file("CurlSSL", "cacert.pem",
                                                  package = "RCurl"))),
         envir = .GlobalEnv)
  })
}
myfn <- function(fn.file) {
  source_https(paste(mydir, 'functions/', fn.file, sep = ''))
}
```

The author is neither a programmer nor a computer scientist by training; any suggestions on how to make this document or the suite of functions it overviews would be most welcome.

## Network 1: Southern women

In their book *Deep South*, social anthropologists Davis, Gardner, and Gardner presented, amidst other more typical tables of sociometric data, tables of coattendance at distinct events by three groups of acquainted women in the town of Natchez, Mississippi. One of these, labeled Clique A, is depicted in Fig. 11 on p. 209. The clique consists of five women, designated "Miss A" through "Miss E", each of whom attended some of five recorded events, which we refer to as bridge, dinner, movies, dance, and visiting. The attendance table is reproduced in the file "DGG\_Clique\_A.csv", which we load into R directly as a graph:


```r
women <- graph.incidence(as.matrix(mycsv('DGG_Clique_A.csv', row.names = 1)))
V(women)$type <- !V(women)$type
```

Since the graph is bipartite, we can get all the incidence information we need from one corner of the full adjacency matrix. Due to the structure of the file and the import method, the actor nodes are listed first and the event nodes second:


```r
women.mat <- get.adjacency(women)[1:5, 6:10]
print(women.mat)
```

```
## 5 x 5 sparse Matrix of class "dgCMatrix"
##        Bridge Dinner Movies Dance Visiting
## Miss A      1      .      1     1        .
## Miss B      .      .      1     1        .
## Miss C      1      1      .     .        1
## Miss D      1      1      1     .        .
## Miss E      .      1      .     1        1
```

First let's visualize the network, using the Fruchterman-Reingold algorithm and the visual scheme from [Opsahl's paper][]:

[Opsahl's paper] http://toreopsahl.com/2011/12/21/article-triadic-closure-in-two-mode-networks-redefining-the-global-and-local-clustering-coefficients/ "Opsahl"


```r
plot(women, layout = layout.fruchterman.reingold(women, niter = 100),
     vertex.color = rep(c('SkyBlue2', 'lightcoral'), times = c(5, 5)),
     vertex.shape = rep(c('circle', 'square'), times = c(5, 5)),
     edge.width = 2, edge.color = 'black',
     vertex.label = c(LETTERS[1:5], 1:5),
     vertex.label.family = 'sans', vertex.label.color = 'black')
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 

The layout helps distinguish the nodes but offers only indirect insight into the relative rates of attendance of the individuals or events. Alternatively, we can visualize the nodes along parallel lines:


```r
plot(women, layout = matrix(c(rep(seq(-1, 1, length.out = 5), times = 2),
                              rep(c(1, -1), each = 5)), nc = 2),
     vertex.color = rep(c('SkyBlue2', 'lightcoral'), times = c(5, 5)),
     vertex.shape = rep(c('circle', 'square'), times = c(5, 5)),
     edge.width = 2, edge.color = 'black',
     vertex.label = c(LETTERS[1:5], 1:5),
     vertex.label.family = 'sans', vertex.label.color = 'black')
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5.png) 

The individuals and events are pretty evenly connected--three ties each, except for one individal (Miss B) and one event (visiting) having two ties each.

This social network is just large enough exhibit a diversity of triads and just small enough to allow us to examine them all. We can view the (undirected) triad census of the one-mode projection, taking the second of the bipartite projections corresponding to the nodes of type 1 (actors) rather than 0 (events):


```r
myfn('simple.triad.census.R')
```

```
## $`https://raw.githubusercontent.com/corybrunson/triadic/master/functions/simple.triad.census.R`
## function (graph) 
## {
##     tc <- triad.census(as.directed(graph))
##     if (is.nan(tc[1])) 
##         tc[1] <- choose(vcount(graph), 3) - sum(tc, na.rm = TRUE)
##     tc[c(1, 3, 11, 16)]
## }
```

```r
simple.triad.census(bipartite.projection(women)[[2]])
```

```
## [1] 0 0 3 7
```

We have no disconnected triples at all; only three 'wedges' or 'vees' and seven 'triangles'. But these probably exhibit some diversity of their own that is lost in the projection. We can take a look at the two-mode triad census using the (currently cumbersome) function 'two.mode.triad.census':


```r
myfn('partition.bijections.R')
```

```
## $`https://raw.githubusercontent.com/corybrunson/triadic/master/functions/partition.bijections.R`
## function (lambda) 
## {
##     combination.position(partition.combination(lambda))
## }
```

```r
myfn('two.mode.triad.census.R')
```

```
## $`https://raw.githubusercontent.com/corybrunson/triadic/master/functions/two.mode.triad.census.R`
## function (bigraph, type = 1, max.wt = Inf) 
## {
##     V(bigraph)$name <- V(bigraph)
##     if (vcount(bigraph) == 0) 
##         return(list(pw = 0, C = matrix(0, nr = 0, nc = 0), D = matrix(0, 
##             nr = 0, nc = 2)))
##     h <- bipartite.projection(bigraph, multiplicity = T)[[1 + 
##         type]]
##     vl <- do.call(cbind, lapply(V(h)[1:(vcount(h) - 1)], function(v) {
##         d2 <- as.numeric(V(h)[which(shortest.paths(h, v, (v + 
##             1):vcount(h), weights = NA) == 2) + v])
##         gasp <- get.all.shortest.paths(h, v, d2, weights = NA)[[1]]
##         do.call(cbind, gasp[sapply(gasp, length) == 3])
##     }))
##     tl <- do.call(cbind, cliques(h, min = 3, max = 3))
##     pw <- if (max.wt == Inf) 
##         max(E(h)$weight)
##     else if (!is.null(max.wt)) 
##         max.wt
##     else quantile(E(h)$weight, 0.95)
##     C <- as.data.frame(matrix(0, nr = choose(pw + 3, 3), nc = pw + 
##         1))
##     D <- matrix(0, nr = 0, nc = 2)
##     pb <- txtProgressBar(min = 1, max = ecount(h), style = 3)
##     for (e in 1:ecount(h)) {
##         i <- partition.position(c(E(h)[e]$weight, 0, 0)) + 1
##         if (E(h)[e]$weight > pw) {
##             rep.count <- vcount(h) - length(unique(unlist(neighborhood(h, 
##                 1, nodes = get.edge(h, e)))))
##             stopifnot(rep.count >= 0)
##             if (rep.count > 0) 
##                 D <- rbind(D, t(sapply(1:rep.count, function(x) c(i + 
##                   1, 1))))
##         }
##         else {
##             C[i, 1] <- C[i, 1] + vcount(h) - length(unique(unlist(neighborhood(h, 
##                 1, nodes = get.edge(h, e)))))
##         }
##         setTxtProgressBar(pb, e)
##     }
##     close(pb)
##     if (!is.null(vl)) {
##         bar <- dim(vl)[2] > 1
##         if (bar) 
##             pb <- txtProgressBar(min = 1, max = dim(vl)[2], style = 3)
##         for (v in 1:dim(vl)[2]) {
##             lamb <- sort(c(E(h, P = vl[1:2, v])$weight, E(h, 
##                 P = vl[2:3, v])$weight), decreasing = TRUE)
##             i <- partition.position(c(lamb, 0)) + 1
##             if (lamb[1] > pw) {
##                 D <- rbind(D, c(i, 1))
##             }
##             else {
##                 C[i, 1] <- C[i, 1] + 1
##             }
##             if (bar) 
##                 setTxtProgressBar(pb, v)
##         }
##         if (bar) 
##             close(pb)
##     }
##     if (!is.null(tl)) {
##         bar <- dim(tl)[2] > 1
##         if (bar) 
##             pb <- txtProgressBar(min = 1, max = dim(tl)[2], style = 3)
##         for (t in 1:dim(tl)[2]) {
##             tr <- as.numeric(V(h)$name[tl[, t]])
##             s <- induced.subgraph(bigraph, vids = unique(unlist(neighborhood(bigraph, 
##                 1, tr))))
##             tw <- length(which(degree(s) * (V(s)$type != type) == 
##                 3))
##             lambda <- sort(c(E(h, P = tl[1:2, t])$weight - tw, 
##                 E(h, P = tl[2:3, t])$weight - tw, E(h, P = tl[c(1, 
##                   3), t])$weight - tw), decreasing = TRUE)
##             i <- partition.position(lambda) + 1
##             if (lambda[1] > pw || tw > pw) {
##                 D <- rbind(D, c(i, 1))
##             }
##             else {
##                 C[i, tw + 1] <- C[i, tw + 1] + 1
##             }
##             if (bar) 
##                 setTxtProgressBar(pb, t)
##         }
##         if (bar) 
##             close(pb)
##     }
##     C[1, 1] <- C[1, 1] + choose(vcount(h), 3) - sum(C) - dim(D)[1]
##     C <- C[, 1:max(which(colSums(C) > 0))]
##     if (dim(D)[1] > 0) 
##         D <- aggregate(rep(1, dim(D)[1]), by = list(D[, 1], D[, 
##             2]), "sum")
##     return(list(pw, C, D))
## }
```

```r
two.mode.triad.census(women)
```

```
##   |                                                                         |                                                                 |   0%  |                                                                         |========                                                         |  12%  |                                                                         |================                                                 |  25%  |                                                                         |========================                                         |  38%  |                                                                         |================================                                 |  50%  |                                                                         |=========================================                        |  62%  |                                                                         |=================================================                |  75%  |                                                                         |=========================================================        |  88%  |                                                                         |=================================================================| 100%
##   |                                                                         |                                                                 |   0%  |                                                                         |================================                                 |  50%  |                                                                         |=================================================================| 100%
##   |                                                                         |                                                                 |   0%  |                                                                         |===========                                                      |  17%  |                                                                         |======================                                           |  33%  |                                                                         |================================                                 |  50%  |                                                                         |===========================================                      |  67%  |                                                                         |======================================================           |  83%  |                                                                         |=================================================================| 100%
```

```
## [[1]]
## [1] 2
## 
## [[2]]
##    V1 V2
## 1   0  0
## 2   0  1
## 3   0  3
## 4   1  0
## 5   0  0
## 6   3  0
## 7   2  0
## 8   0  0
## 9   0  0
## 10  0  0
## 
## [[3]]
##      [,1] [,2]
```

(Note: This function needs to be cleaned up quite a bit.)
