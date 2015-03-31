---
title: "Triadic analysis of the southern women datasets"
author: "Jason Cory Brunson"
date: "2015-03-31"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{Triadic analysis of the southern women datasets}
  %\VignetteEngine{knitr::rmarkdown}
  \usepackage[utf8]{inputenc}
---



This vignette uses several tools from the `bitriad` package (which depends on [`igraph`](http://igraph.org/r/) and introduces no new classes) to study two small social groups inferred from event coattendence.

### Source

In their book [*Deep South*](http://books.google.com/books?id=Q3b9QTOgLFcC), five social anthropologists presented a comprehensive case study of the American caste system as it operated in a rural town in Mississippi. Among the data they collected were several tables of attendance at various events by groups of acquainces. Three of these are presented in the book. One, labeled Clique A (p. 209, Fig. 11) consists of five women, designated "Miss A" through "Miss E", and five activities, described as bridge, dinner, movies, dance, and visiting, some subset of the women participated in each of which. The attendance records serve as the adjacency matrix for the `igraph` object `ddggs.clique`:


```r
library(bitriad)
data(ddggs.clique)
get.incidence(ddggs.clique)
```

```
##        Bridge Dinner Movies Dance Visiting
## Miss A      1      0      1     1        0
## Miss B      0      0      1     1        0
## Miss C      1      1      0     0        1
## Miss D      1      1      1     0        0
## Miss E      0      1      0     1        1
```

To be recognized as bipartite, the object's vertices must have a logical `type` attribute. The tools of `bitriad` interpret the nodes of type `FALSE` as actors and those of type `TRUE` as events, in keeping with the convention in `igraph` of adjacency matrix rows corresponding to type-`FALSE` vertices.

### Visualization

The function `anPlotSpecs` imbues an affiliation network with attributes designed to help visually distinguish between actors and events. In particular, it scales vertex sizes with the reciprocal of the vertex count (up to 100), and it adopts shape and color conventions from the literature.



































