# Project an affiliation network onto its actors

These functions use
[bipartite_projection](https://r.igraph.org/reference/bipartite_projection.html)
to compute the projections of an affiliation network onto the actor or
event nodes.

## Usage

``` r
mode_projection(graph, mode = 1, name = "name")

actor_projection(graph, ...)

event_projection(graph, ...)

actor.projection(graph, ...)

event.projection(graph, ...)
```

## Arguments

- graph:

  An affiliation network.

- mode:

  Numeric or character; whether to project onto actors (`1` or
  `"actors"`) or onto events (`2` or `"events"`).

- name:

  Character; the attribute of the actor or event nodes in `graph` to use
  as names for the nodes in the projection. If `NA`, node IDs are
  converted to characters and used. If `NULL`, no names are assigned.

- ...:

  Arguments passed to `mode_projection`.

## See also

Original **igraph** functions:
[bipartite_projection](https://r.igraph.org/reference/bipartite_projection.html)

## Examples

``` r
data(chicago1960s)
tab <- table(V(chicago1960s)$type)
proj <- actor_projection(chicago1960s)
vcount(proj) == tab[1]
#> FALSE 
#>  TRUE 
proj <- event_projection(chicago1960s)
vcount(proj) == tab[2]
#> TRUE 
#> TRUE 
```
