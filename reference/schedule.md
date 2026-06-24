# Actors and their shared events

Given an affiliation network and a vector of actor node IDs, produce the
induced subgraph on the actor nodes together with all event nodes
incident to at least two of them. This is called the actors' *schedule*.

## Usage

``` r
schedule(graph, actors = V(graph)[V(graph)$type == FALSE])
```

## Arguments

- graph:

  An affiliation network.

- actors:

  A vector of actor nodes in `graph`.

## Value

An `igraph` object induced from the input `graph`.

## See also

Other modal queries and manipulations:
[`dualize()`](http://corybrunson.github.io/bitriad/reference/dualize.md),
[`mode_addition`](http://corybrunson.github.io/bitriad/reference/mode_addition.md),
[`mode_counts`](http://corybrunson.github.io/bitriad/reference/mode_counts.md),
[`modes`](http://corybrunson.github.io/bitriad/reference/modes.md)

## Examples

``` r
data(women_clique)
schedule(women_clique, actors = V1(women_clique)[seq(3)])
#> IGRAPH 021258e UN-B 6 6 -- 
#> + attr: type (v/l), name (v/c)
#> + edges from 021258e (vertex names):
#> [1] Miss A--Bridge Miss A--Movies Miss A--Dance  Miss B--Movies Miss B--Dance 
#> [6] Miss C--Bridge
```
