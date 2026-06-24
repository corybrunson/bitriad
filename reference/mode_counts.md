# Count the actors and events in an affiliation network

These functions return the number of actors (nodes with `type` attribute
`FALSE`) or events (`TRUE`) in an affiliation network.

## Usage

``` r
actor_count(graph)

event_count(graph)

actor.count(graph)

event.count(graph)
```

## Arguments

- graph:

  An affiliation network.

## Value

An integer.

## See also

Original **igraph** functions:
[`vcount()`](https://r.igraph.org/reference/gorder.html),
[`ecount()`](https://r.igraph.org/reference/gsize.html)

Other modal queries and manipulations:
[`dualize()`](http://corybrunson.github.io/bitriad/reference/dualize.md),
[`mode_addition`](http://corybrunson.github.io/bitriad/reference/mode_addition.md),
[`modes`](http://corybrunson.github.io/bitriad/reference/modes.md),
[`schedule()`](http://corybrunson.github.io/bitriad/reference/schedule.md)

## Examples

``` r
data(chicago1960s)
actor_count(chicago1960s)
#> [1] 20
event_count(chicago1960s)
#> [1] 24
```
