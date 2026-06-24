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

## Value

The input `graph` with aesthetic node and link attributes (re)set.

## Examples

``` r
library(igraph)
data(women_clique)
data(whigs)
for (g in list(women_clique, whigs)) {
    plot(prettify_an(g))
}

```
