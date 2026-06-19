# Triad tallies

These functions are called by the full triad census to handle triads of
different types using the projection onto actor nodes. The name of each
function indicates the number of edges that appear among the three
actors of the triad in the projection. (Zero-edge triads do not need to
be tallied; the total number of triads is easily calculated, and the
difference between this number and the total number of triads with edges
gives the number of triads without.)

## Usage

``` r
connectedTriples(bigraph, graph = actor_projection(bigraph, name = "id"))

oneTiedTriads(graph)

twoTiedTriads(graph)

threeTiedTriads(bigraph, graph = actor_projection(bigraph, name = "id"))
```

## Arguments

- bigraph:

  The ambient affiliation network from which `graph` is projected

- graph:

  A one-mode network

## See also

Other triad census functions:
[`project_census()`](http://corybrunson.github.io/bitriad/reference/project_census.md),
[`triad_census()`](http://corybrunson.github.io/bitriad/reference/triad_census.md),
[`triad_closure_from_census()`](http://corybrunson.github.io/bitriad/reference/triad_closure_from_census.md)
