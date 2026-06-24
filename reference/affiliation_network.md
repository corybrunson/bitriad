# Affiliation network structure

Test `igraph` objects for affiliation network structure or impose such
structure if possible.

## Usage

``` r
is_an(graph)

is.an(graph)

as_an(graph, map_type = FALSE)

as.an(graph, map_type = FALSE)
```

## Arguments

- graph:

  An `igraph` object.

- map_type:

  Logical; whether to add a `type` attribute for bipartite structure if
  `graph` admits one.

## Value

For `is_an()`, a logical value; for `as_an()`, the input `graph` with a
`"type"` attribute.

## Details

An affiliation network is a bipartite graph whose nodes are classified
as actors and events in such a way that all links are between actors and
events. The function
[`is_bipartite()`](https://r.igraph.org/reference/is_bipartite.html)
tests an `igraph` object for a `type` attribute, which is intended to
bipartition of the nodes. It does not test whether the links respect
this partition. The function `is_an` tests this, as well as the
condition that actor nodes precede event nodes in their node IDs, which
simplifies some other functions. The function `as_an` coerces an
`igraph` object to an affiliation network by verifying that the object
is bipartite and minimally permuting the node IDs. If `graph` has no
`type` attribute and `map_type` is `FALSE`, then `as_an` throws an
error; if `map_type` is `TRUE`, then `as_an` calls
[`bipartite_mapping()`](https://r.igraph.org/reference/bipartite_mapping.html)
to add a logical `type` attribute that takes the value `FALSE` at the
first node (by node ID) in each connected component and `TRUE` or
`FALSE` at the remaining nodes. (If this cannot be done, an error is
thrown.)

## See also

Original **igraph** functions:
[`is_igraph()`](https://r.igraph.org/reference/is_igraph.html)

Other network testing and coercion:
[`dynamic_an`](http://corybrunson.github.io/bitriad/reference/dynamic_an.md)

## Examples

``` r
graph <- make_graph(c( 1,2, 1,4, 1,6, 3,6, 5,6, 7,8, 9,8 ))
is_an(graph)
#> [1] FALSE
# as_an(graph) # throws an error
an_graph <- as_an(graph, map_type = TRUE)
#> Warning: Graph is directed; collapsing directed links.
is_an(an_graph)
#> [1] TRUE
```
