# Affiliation network structure

Test `igraph` objects for affiliation network structure or impose such
structure if possible.

## Usage

``` r
is_an(graph)

is.an(graph)

as_an(graph, add.type.attribute = FALSE)

as.an(graph, add.type.attribute = FALSE)
```

## Arguments

- graph:

  An `igraph` object.

- add.type.attribute:

  Logical; whether to introduce a `type` attribute if `graph` has none
  before testing for bipartite structure.

## Details

An affiliation network is a bipartite graph whose nodes are classified
as actors and events in such a way that all links are between actors and
events. The function
[is_bipartite](https://r.igraph.org/reference/is_bipartite.html) tests
an `igraph` object for a `type` attribute, which is intended to
bipartition of the nodes. It does not test whether the links respect
this partition. The function `is_an` tests this, as well as the
condition that actor nodes precede event nodes in their node IDs, which
simplifies some other functions. The function `as_an` coerces an
`igraph` object to an affiliation network by verifying that the object
is bipartite and minimally permuting the node IDs. If `graph` has no
`type` attribute and `add.type.attribute` is `FALSE`, then `as_an`
throws an error; if `add.type.attribute` is `TRUE`, then `as_an`
introduces a logical `type` attribute that takes the value `FALSE` at
the first node (by node ID) in each connected component and `TRUE` or
`FALSE` at the remaining nodes according as they are an odd or even
number of hops from the first.

## See also

Original **igraph** functions:
[is_igraph](https://r.igraph.org/reference/is_igraph.html)

Other network testing and coercion:
[`dynamic_an`](http://corybrunson.github.io/bitriad/reference/dynamic_an.md)
