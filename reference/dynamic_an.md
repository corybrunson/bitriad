# Dynamic affiliation network structure

An affiliation network is *dynamic*, for present purposes, if its event
nodes have time stamps, recorded as a numeric vertex attribute `"time"`
in non-decreasing order.

## Usage

``` r
is_dynamic_an(graph)

is.dyn(graph)

as_dynamic_an(graph, use.time.attribute = "time", add.time.attribute = FALSE)
```

## Arguments

- graph:

  An affiliation network.

- use.time.attribute:

  Character; the vertex attribute to coerce to numeric if necessary and
  use as the `"time"` attribute.

- add.time.attribute:

  Logical; whether, if `graph` has no attribute `use.time.attribute`, to
  introduce an artificial `"time"` attribute taking the values
  `1:event_count(graph)`, reflecting the order of the event node IDs.

## See also

Other network testing and coercion:
[`affiliation_network`](http://corybrunson.github.io/bitriad/reference/affiliation_network.md)
