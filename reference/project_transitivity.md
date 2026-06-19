# Affiliation network clustering coefficients

Each clustering coefficient can be defined as the proportion of "wedges"
that are "closed", for suitable definitions of both terms. The main
function, transitivity_an, calls one of the wedge functions and computes
the global or local clustering coefficient of the given affiliation
network, and if the local, then at the given nodes.
(`project_transitivity` cheats by using the native `transitivity` but
produces output consistent with the other variants of
`transitivity_an`.)

## Usage

``` r
project_transitivity(graph, type = "global", vids = which(!V(graph)$type))

project.transitivity(graph, type = "global", vids = which(!V(graph)$type))
```

## Arguments

- graph:

  An affiliation network.

- type:

  The type of clustering coefficient (defaults to "global")

- vids:

  A subset of actor node ids at which to evaluate the local clustering
  coefficient.

## See also

Other triad closure functions:
[`dynamic_triad_closure()`](http://corybrunson.github.io/bitriad/reference/dynamic_triad_closure.md),
[`transitivity_an()`](http://corybrunson.github.io/bitriad/reference/transitivity_an.md),
[`triad_closure()`](http://corybrunson.github.io/bitriad/reference/triad_closure.md),
[`triad_closure_from_census()`](http://corybrunson.github.io/bitriad/reference/triad_closure_from_census.md)
