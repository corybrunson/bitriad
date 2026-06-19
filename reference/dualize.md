# Take the dual of an affiliation network

This function obtains the dual of an affiliation network, in which the
actors and events have swapped roles. To do this, it negates the logical
values of the node `type` attribute and reorders the node ids
accordingly.

## Usage

``` r
dualize(graph)

dual_an(graph)

dual.an(graph)
```

## Arguments

- graph:

  An affiliation network.

## See also

Other modal queries and manipulations:
[`mode_addition`](http://corybrunson.github.io/bitriad/reference/mode_addition.md),
[`mode_counts`](http://corybrunson.github.io/bitriad/reference/mode_counts.md),
[`modes`](http://corybrunson.github.io/bitriad/reference/modes.md),
[`schedule()`](http://corybrunson.github.io/bitriad/reference/schedule.md)

## Examples

``` r
data(women_clique)
tab <- table(V(women_clique)$type)
#> This graph was created by an old(er) igraph version.
#> ℹ Call `igraph::upgrade_graph()` on it to use with the current igraph version.
#> For now we convert it on the fly...
proj <- actor_projection(dualize(women_clique))
vcount(proj) == tab[2]
#> TRUE 
#> TRUE 
```
