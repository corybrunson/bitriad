# Convenient plotting aesthetics for affiliation networks

Given an affiliation network, assign the node and link aesthetics to
values that produce a neater visualization through
[plot.igraph](https://r.igraph.org/reference/plot.igraph.html) than the
**igraph** defaults.

## Usage

``` r
prettify_an(graph)

prettify.an(graph)
```

## Arguments

- graph:

  An affiliation network.

## Examples

``` r
library(igraph)
data(women_clique)
data(whigs)
for (g in list(women_clique, whigs)) {
    plot(prettify_an(g))
}
#> This graph was created by an old(er) igraph version.
#> ℹ Call `igraph::upgrade_graph()` on it to use with the current igraph version.
#> For now we convert it on the fly...

#> This graph was created by an old(er) igraph version.
#> ℹ Call `igraph::upgrade_graph()` on it to use with the current igraph version.
#> For now we convert it on the fly...
```
