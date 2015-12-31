---
title: "Triadic analysis of the southern women datasets"
author: "Jason Cory Brunson"
date: "2015-12-31"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{Triadic analysis of the southern women datasets}
  %\VignetteEngine{knitr::rmarkdown}
  \usepackage[utf8]{inputenc}
---



This vignette uses several tools from the `bitriad` package (which depends on [`igraph`](http://igraph.org/r/) and introduces no new classes) to study two small social groups inferred from event coattendence.

### Source

In their book [*Deep South*](http://books.google.com/books?id=Q3b9QTOgLFcC), five social anthropologists presented a comprehensive case study of the American caste system as it operated in a rural town in Mississippi. Among the data they collected were several tables of attendance at various events by groups of acquainces. Three of these are presented in the book. One, labeled Clique A (p. 209, Fig. 11) consists of five women, designated "Miss A" through "Miss E", and five activities, described as bridge, dinner, movies, dance, and visiting, some subset of the women participated in each of which. The attendance records serve as the adjacency matrix for the `igraph` object `women.clique`:


```r
library(bitriad)
data(women.clique)
get.incidence(women.clique)
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

The function `prettify.an` imbues an affiliation network with attributes designed to help visually distinguish between actors and events. In particular, it scales vertex sizes with the reciprocal of the vertex count (up to 100), and it adopts shape and color conventions from the literature.

<img src="figure/unnamed-chunk-3-1.png" title="plot of chunk unnamed-chunk-3" alt="plot of chunk unnamed-chunk-3" style="display: block; margin: auto;" />

The layout reveals a symmetry between the actors and the events: Exchanging Miss A and Event 2, Miss B and Event 5, and so on yields a graph isomorphism. Thus any structural information we learn about the actors in this network can be flipped into equivalent information about the events. Such symmetry is unusual, but the *duality* between actors and events, which would allow us to use actor-centric tools in the study of events (and vice-versa, though this will not be done here), is [of central importance](http://www.rci.rutgers.edu/~pmclean/mcleanp_01_920_313_breiger_duality.pdf) to the study of affiliation networks.

### Triad census

This social network is just large enough to house a diversity of triads and just small enough for us to examine them all in detail. Classically, the *triad census* refers to the distribution of triads of 16 isomorphism classes throughout a *directed, unipartite* network. The women's clique is neither, but we can view a simplified (undirected) version of the triad census on its *projection*--the network of five women with acquaintanceship ties inferred from their shared activities. There are only four isomorphism classes of undirected triads, distinguished by the number of edges (0 through 3) among the three nodes:


```r
women.clique.proj <- actor.projection(women.clique)
tc <- simple.triad.census(women.clique.proj, add.names = TRUE)
tc
```

```
## 0 1 2 3 
## 0 0 3 7
```

There are no null (no edges) or disconnected (one edge) triples among the women, only three "vees" (two edges) and seven "triangles" (three edges). But these categories, especially the last, ignore higher-order structure in the original affiliation network. This structure--three actors at a time and any events from which connections among them are inferred--is the basis for the **(full) affiliation network triad census**. Given an affiliation network, this census tallies all triples of actors by the number of "inclusive" events they all three attended and the distribution among them of "exclusive" events only attended by two.[^3]

[^3]: My naming convention for several functions mimics that for a hypothetical class `an` (for "affiliation network"), which may be appropriate for this package but is not currently defined.)


```r
antc <- triad.census.an(women.clique, add.names = TRUE)
antc
```

```
##         0 1
## (0,0,0) 0 0
## (1,0,0) 0 1
## (1,1,0) 0 3
## (1,1,1) 1 0
## (2,0,0) 0 0
## (2,1,0) 3 0
## (2,1,1) 2 0
## (2,2,0) 0 0
## (2,2,1) 0 0
## (2,2,2) 0 0
```

The arrangement is not so intuitive. The rows reflect the distribution of exclusive events, and the columns indicate the number of inclusive events; for instance, Miss A and Miss B attended two events (movies and dance) without Miss C, and Miss A and Miss C attended one event (bridge) without Miss B, while Miss B and Miss C attended no events together. The exclusive events thus form the (ordered) sequence (2,1,0), so the triad (A, B, C) is counted in the sixth row and first column (zero inclusive events) of the matrix. (The appropriate row is the index of (2 ≥ 1 ≥ 0) in the enumeration scheme provided by the *revolving door algorithm*.

As networks grow, this scheme quickly becomes ridiculous. There are, however, intermediate schemes that capture valuable information that is lost to the simple census. Consider the **structural triad census**, which collapses duplicate events (relative to the triad), replacing the counts with (numeric) indicators, but still distinguishes inclusive from exclusive:


```r
antc.proj <- project.census(antc, add.names = TRUE)
antc.proj$structural
```

```
##   0 1
## 0 0 0
## 1 0 1
## 2 3 3
## 3 3 0
```

The column indicates the existence of an inclusive event; the row indicates the number of non-duplicate exclusive events (0, 1, 2, or 3). The simple triad census can be recovered from either of these higher-order censuses:


```r
cbind(tc, antc.proj$simple, project.census(antc.proj$structural)$simple)
```

```
##   tc    
## 0  0 0 0
## 1  0 0 0
## 2  3 3 3
## 3  7 7 7
```

### Global clustering coefficients

The classical (global) clustering coefficient may be defined for a traditional network as the proportion of "vees" that are "closed"--that is, whose end nodes are tied. Since every triad of three edges counts thrice as a closed vee, while every two-edged triad constitutes a single open vee, we can compute the clustering coefficient of the projection from the simple census:


```r
C <- unname(3 * tc[4]/(tc[3] + 3 * tc[4]))
C
```

```
## [1] 0.875
```

The value tells us what proportion of the time two coattendees with the same third woman have themselves been to an activity together. The clustering coefficient has proven a valuable indicator of triadic closure--the tendency for shared connections to lead to direct connections, i.e. for "friends of friends" to in fact be (or become) "friends".

The paper discusses in detail two alternative clustering coefficients specifically designed for affiliation networks. The first is the *[Opsahl](http://toreopsahl.com/2011/12/21/article-triadic-closure-in-two-mode-networks-redefining-the-global-and-local-clustering-coefficients/) clustering coefficient*, the first proposed measure of triadic closure specific to affiliation networks. (This distinction ignores previous bipartite clustering coefficients that were not based on triples of actors.) The second is dubbed the **exclusive clustering coefficient** because it depends only on the exclusive events any triad. Analogously to above, both of these diagnostics is recoverable from the triad census.[^2]

[^2]: The calculations are wrapped into `transitivity.census()`, which can also handle uniformity, structural, and simple census input, provided the desired flavor of transitivity can be recovered therefrom.


```r
C.vec <- c(C = transitivity.census(antc, scheme = "full", flavor = "classical"), 
    OpsahlC = transitivity.census(antc, scheme = "full", flavor = "opsahl"), 
    exclC = transitivity.census(antc, scheme = "full", flavor = "exclusive"))
C.vec
```

```
##         C   OpsahlC     exclC 
## 0.8750000 0.6111111 0.6000000
```

In fact, the exclusive clustering coefficient can be calculated from the structural census, since its definition does not depend on the presence of duplicate events:


```r
stc <- antc.proj$structural
3 * sum(stc[4, ])/(sum(stc[3, ]) + 3 * sum(stc[4, ]))
```

```
## [1] 0.6
```

### Local clustering coefficients

So far we have only measured triadic closure network-wide; that is, we have been looking at *global* properties. But triadic analysis has always taken place at the interface between micro and macro. The Davis/Holland/Leinhardt studies used theoretical micro assumptions to predict empirically testable macro structure, and the global clustering coefficient was a macro counterpart to the original (local) clustering coefficient of Watts and Strogatz. Having viewed the southern women through this global lens, we now turn to the local.

The classical local clustering coeffiicent is the proportion of pairs of an actor's neighbors who are themselves neighbors. From the image above we can see that the only pair of women not linked through at least one event are Miss B and Miss C. This means that the only local clustering coefficients we'll observe are 5/6 (for women who count Mss. B and C among their neighobrs, i.e. everyone except Mss. B and C) and 1 (for Mss. B and C):


```r
C.local <- transitivity(women.clique.proj, type = "local")
names(C.local) <- V(women.clique.proj)$name
C.local
```

```
##    Miss A    Miss B    Miss C    Miss D    Miss E 
## 0.8333333 1.0000000 1.0000000 0.8333333 0.8333333
```

Our higher-order candidates (Opsahl and exclusive) are implemented using `transitivity.an()`, a shell for the required "wedge" function that counts the open and closed wedges at a node. It is this function that determines the species of triadic closure to be calculated. Once the wedges are tallied in a 2-column matrix, `transitivity.an` computes from them whatever summary statistic is desired. For example, here are the "exclusive" wedges among the five women:


```r
exclWedges <- excl.transitivity(women.clique, type = "")
exclWedges
```

```
##      [,1] [,2]
## [1,]    4    2
## [2,]    1    1
## [3,]    2    1
## [4,]    4    2
## [5,]    4    3
```

From these, the global and local exclusive clustering coefficiencs may be recovered:


```r
sum(exclWedges[, 2])/sum(exclWedges[, 1])  # global
```

```
## [1] 0.6
```

```r
exclWedges[, 2]/exclWedges[, 1]  # local
```

```
## [1] 0.50 1.00 0.50 0.50 0.75
```

(Note the absence of multiplication by 3 in the global calculation; each triangle contributes 3 closed wedges to `exclWedges`.) The three local clustering coefficients provide an illustrative comparison:


```r
C.local.dat <- cbind(C = C.local, OpsahlC = opsahl.transitivity(women.clique, 
    type = "local"), exclC = excl.transitivity(women.clique, type = "local"))
rownames(C.local.dat) <- V(women.clique.proj)$name
C.local.dat
```

```
##                C   OpsahlC exclC
## Miss A 0.8333333 0.5000000  0.50
## Miss B 1.0000000 0.6666667  1.00
## Miss C 1.0000000 0.6666667  0.50
## Miss D 0.8333333 0.6000000  0.50
## Miss E 0.8333333 0.7142857  0.75
```

(Paraphrase the example in the paper.)

### Wedge-dependent local clustering

One [thoroughly documented](http://arxiv.org/abs/cond-mat/0211528) property of social networks is the inverse relationship between local connectivity and local clusterability. 

(Rewrite based on paper.)

Here's the relationship between connectivity and clusterability in Clique A:


```r
ddc <- data.frame(k = degree(women.clique.proj), C = transitivity(women.clique.proj, 
    type = "local"))
print(ddc)
```

```
##        k         C
## Miss A 4 0.8333333
## Miss B 3 1.0000000
## Miss C 3 1.0000000
## Miss D 4 0.8333333
## Miss E 4 0.8333333
```

```r
plot(aggregate(ddc$C, by = list(ddc$k), FUN = mean), pch = 19, type = "b", main = "Degree-dependent local clustering", 
    xlab = "Degree", ylab = "Mean conditional local clustering coefficient")
```

<img src="figure/unnamed-chunk-15-1.png" title="plot of chunk unnamed-chunk-15" alt="plot of chunk unnamed-chunk-15" style="display: block; margin: auto;" />

There is little insight to be gleaned here; a more heterogeneous network is required. Though the curve at least proceeds in the expected direction. The same research team recorded another table of women and events, helpfully labeled Group I (p. 148)[^1].

[^1]: These data are available from several sources, though one error (the substitution of `MYRNA` for `MYRA`) crept into an early digitization and has been widely copied since; moreover, to my knowledge, no other digitizations include the date information from the original table.

For this network, we'll use the bipartite layout, though some interesting structure is discernible from the Fruchterman-Reingold layout:


```r
data(women.group)
women.group <- prettify.an(women.group)
V(women.group)$label <- substr(V(women.group)$name, 1, ifelse(V(women.group)$type, 
    5, 2))
V(women.group)$label.color <- "white"
set.seed(2)
plot(women.group, layout = layout.bipartite(women.group))
```

<img src="figure/unnamed-chunk-16-1.png" title="plot of chunk unnamed-chunk-16" alt="plot of chunk unnamed-chunk-16" style="display: block; margin: auto;" />

As hoped, the women of Group I exhibit a range of connectivity and clusterability (in the classical sense):


```r
women.group.proj <- actor.projection(women.group)
ddc2 <- data.frame(k = degree(women.group.proj), C = transitivity(women.group.proj, 
    type = "local"))
print(ddc2)
```

```
##            k         C
## Evelyn    17 0.8970588
## Laura     15 0.9619048
## Theresa   17 0.8970588
## Brenda    15 0.9619048
## Charlotte 11 1.0000000
## Frances   15 0.9619048
## Eleanor   15 0.9619048
## Pearl     16 0.9333333
## Ruth      17 0.8970588
## Verne     17 0.8970588
## Myra      16 0.9333333
## Katherine 16 0.9333333
## Sylvia    17 0.8970588
## Nora      17 0.8970588
## Helen     17 0.8970588
## Dorothy   16 0.9333333
## Olivia    12 1.0000000
## Flora     12 1.0000000
```

```r
plot(aggregate(ddc2$C, by = list(k = ddc2$k), FUN = mean), pch = 19, type = "b", 
    main = "Degree-dependent local clustering", xlab = "Degree", ylab = "Mean conditional local clustering coefficient")
```

<img src="figure/unnamed-chunk-17-1.png" title="plot of chunk unnamed-chunk-17" alt="plot of chunk unnamed-chunk-17" style="display: block; margin: auto;" />

There is also clearly a trade-off between the number of a woman's acquaintances and the proportion that are also acquainted; perhaps one's capacity for acquaintanceship outpaces one's ability to make introductions and forge new acquaintanceships.

This distribution can be fruitfully generalized to the two-mode setting. What's needed a suitable analog of degree--that is, a measure of local connectivity on which local clustering can be meaningfully conditioned. As suggested by the discussion above, we can adopt local wedge counts, which the `transitivity.an` function returns when neither type (local or global) is specified. Here are the wedge-dependent means and distributions using the Opsahl clustering coefficient:


```r
women.group.wedges <- opsahl.transitivity(women.group, type = "")
women.group.wedges <- cbind(women.group.wedges, women.group.wedges[, 2]/women.group.wedges[, 
    1])
plot(aggregate(women.group.wedges[, 3], by = list(women.group.wedges[, 1]), 
    FUN = mean), pch = 19, type = "b", main = "Wedge-dependent local clustering (Opsahl)", 
    xlab = "Wedges", ylab = "Mean conditional local clustering coefficient")
```

<img src="figure/unnamed-chunk-18-1.png" title="plot of chunk unnamed-chunk-18" alt="plot of chunk unnamed-chunk-18" style="display: block; margin: auto;" />

This plot defies the behavior we saw in the classical case; clusterability neither grows nor declines with connectivity. Here is the equivalent relationship for exclusive clustering:


```r
women.group.wedges <- excl.transitivity(women.group, type = "")
women.group.wedges <- cbind(women.group.wedges, C = women.group.wedges[, 2]/women.group.wedges[, 
    1])
plot(aggregate(women.group.wedges[, 3], by = list(women.group.wedges[, 1]), 
    FUN = mean), pch = 19, type = "b", main = "Wedge-dependent local clustering (exclusive)", 
    xlab = "Wedges", ylab = "Mean conditional local clustering coefficient")
```

<img src="figure/unnamed-chunk-19-1.png" title="plot of chunk unnamed-chunk-19" alt="plot of chunk unnamed-chunk-19" style="display: block; margin: auto;" />

This plot mimics the classical behavior. In the classical case we expect local clustering coefficients to be quite large in tight-knit networks such as those produced for sociological analysis of cliques and communities; the exclusive clustering coefficient, in contrast, takes nearly the full range of possible values, providing a more descriptive metric for dense affiliation networks like Group I.
