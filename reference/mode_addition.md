# Add actor and event nodes

These functions add actor and event nodes (as desired) to a graph while
maintaining its (temporal) affiliation network structure.

## Usage

``` r
add_modes(graph, mode = 1, nv, ..., attr = list(), affiliations = NULL)

add_actors(graph, nv, ..., attr = list(), events = NULL)

add_events(graph, nv, ..., attr = list(), actors = NULL)
```

## Arguments

- graph:

  An affiliation network.

- mode:

  Numeric or character; whether to project onto actors (`1` or
  `"actors"`) or onto events (`2` or `"events"`).

- nv, ..., attr:

  Arguments passed to
  [add_vertices](https://r.igraph.org/reference/add_vertices.html).
  Events added to a dynamic affiliation network should be given time
  attributes.

- affiliations:

  A vector, or list of length `nv` of vectors, of nodes in `graph` of
  mode *not* `mode`, to be linked to the new node(s).

- events:

  A vector, or list of length `nv` of vectors, of event nodes in
  `graph`, to be linked to the new actor(s).

- actors:

  A vector, or list of length `nv` of vectors, of actor nodes in
  `graph`, to be linked to the new event(s).

## See also

Original **igraph** functions:
[add_vertices](https://r.igraph.org/reference/add_vertices.html),
[add_edges](https://r.igraph.org/reference/add_edges.html)

Other modal queries and manipulations:
[`dualize()`](http://corybrunson.github.io/bitriad/reference/dualize.md),
[`mode_counts`](http://corybrunson.github.io/bitriad/reference/mode_counts.md),
[`modes`](http://corybrunson.github.io/bitriad/reference/modes.md),
[`schedule()`](http://corybrunson.github.io/bitriad/reference/schedule.md)

## Examples

``` r
data(women_clique)
plot(prettify_an(add_actors(women_clique, nv = 1, events = c(7, 9))))
#> This graph was created by an old(er) igraph version.
#> ℹ Call `igraph::upgrade_graph()` on it to use with the current igraph version.
#> For now we convert it on the fly...

data(women_group)
plot(prettify_an(women_group))
#> This graph was created by an old(er) igraph version.
#> ℹ Call `igraph::upgrade_graph()` on it to use with the current igraph version.
#> For now we convert it on the fly...

actor_names <- c("Frances", "Dorothy")
cbind(
  dynamic_triad_closure(women_group, type = "local"),
  dynamic_triad_closure(
    add_events(women_group, nv = 1, actors = actor_names, time = 0),
    type = "local"
  ),
  dynamic_triad_closure(
    add_events(women_group, nv = 1, actors = actor_names, time = 367),
    type = "local"
  )
)
#> Warning: 'dynamic_wedges' is experimental.
#> Warning: 'dynamic_wedges' is experimental.
#> Warning: 'dynamic_wedges' is experimental.
#> Warning: 'dynamic_wedges' is experimental.
#> Warning: 'dynamic_wedges' is experimental.
#> Warning: 'dynamic_wedges' is experimental.
#> Warning: 'dynamic_wedges' is experimental.
#> Warning: 'dynamic_wedges' is experimental.
#> Warning: 'dynamic_wedges' is experimental.
#> Warning: 'dynamic_wedges' is experimental.
#> Warning: 'dynamic_wedges' is experimental.
#> Warning: 'dynamic_wedges' is experimental.
#> Warning: 'dynamic_wedges' is experimental.
#> Warning: 'dynamic_wedges' is experimental.
#> Warning: 'dynamic_wedges' is experimental.
#> Warning: 'dynamic_wedges' is experimental.
#> Warning: 'dynamic_wedges' is experimental.
#> Warning: 'dynamic_wedges' is experimental.
#> Warning: 'dynamic_wedges' is experimental.
#> Warning: 'dynamic_wedges' is experimental.
#> Warning: 'dynamic_wedges' is experimental.
#> Warning: 'dynamic_wedges' is experimental.
#> Warning: 'dynamic_wedges' is experimental.
#> Warning: 'dynamic_wedges' is experimental.
#> Warning: 'dynamic_wedges' is experimental.
#> Warning: 'dynamic_wedges' is experimental.
#> Warning: 'dynamic_wedges' is experimental.
#> Warning: 'dynamic_wedges' is experimental.
#> Warning: 'dynamic_wedges' is experimental.
#> Warning: 'dynamic_wedges' is experimental.
#> Warning: 'dynamic_wedges' is experimental.
#> Warning: 'dynamic_wedges' is experimental.
#> Warning: 'dynamic_wedges' is experimental.
#> Warning: 'dynamic_wedges' is experimental.
#> Warning: 'dynamic_wedges' is experimental.
#> Warning: 'dynamic_wedges' is experimental.
#> Warning: 'dynamic_wedges' is experimental.
#> Warning: 'dynamic_wedges' is experimental.
#> Warning: 'dynamic_wedges' is experimental.
#> Warning: 'dynamic_wedges' is experimental.
#> Warning: 'dynamic_wedges' is experimental.
#> Warning: 'dynamic_wedges' is experimental.
#> Warning: 'dynamic_wedges' is experimental.
#> Warning: 'dynamic_wedges' is experimental.
#> Warning: 'dynamic_wedges' is experimental.
#> Warning: 'dynamic_wedges' is experimental.
#> Warning: 'dynamic_wedges' is experimental.
#> Warning: 'dynamic_wedges' is experimental.
#> Warning: 'dynamic_wedges' is experimental.
#> Warning: 'dynamic_wedges' is experimental.
#> Warning: 'dynamic_wedges' is experimental.
#> Warning: 'dynamic_wedges' is experimental.
#> Warning: 'dynamic_wedges' is experimental.
#> Warning: 'dynamic_wedges' is experimental.
#>            [,1]      [,2]      [,3]
#>  [1,] 0.4496124 0.4365079 0.4496124
#>  [2,] 0.6052632 0.6052632 0.6052632
#>  [3,] 0.5144509 0.5058824 0.5144509
#>  [4,] 0.4871795 0.4871795 0.4871795
#>  [5,] 1.0000000 1.0000000 1.0000000
#>  [6,] 0.0000000 0.4782609 0.0000000
#>  [7,] 0.5652174 0.5652174 0.5652174
#>  [8,] 0.4666667 0.4482759 0.4666667
#>  [9,] 0.5000000 0.4939759 0.5000000
#> [10,] 0.5185185 0.5185185 0.5185185
#> [11,] 0.4285714 0.4285714 0.4285714
#> [12,] 0.4285714 0.4285714 0.4285714
#> [13,] 0.5714286 0.5714286 0.5714286
#> [14,] 0.7207792 0.7189542 0.7207792
#> [15,] 0.5714286 0.5714286 0.5714286
#> [16,] 0.0000000 0.3750000 0.0000000
#> [17,] 1.0000000 1.0000000 1.0000000
#> [18,] 1.0000000 1.0000000 1.0000000
```
