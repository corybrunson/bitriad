# Actor and event node iteration and attribute querying and assignment

These functions return actor and event node lists.

## Usage

``` r
V1(graph)

V2(graph)

set_actor_attr(graph, name, index = V1(graph), value)

set_event_attr(graph, name, index = V2(graph), value)

V1(x) <- value

V2(x) <- value
```

## Arguments

- graph:

  An affiliation network.

- name:

  The name of the attribute to set.

- index:

  An optional node sequence to set the attributes of a subset of actor
  or event nodes.

- value:

  The new value of the attribute for all (or `index`) actor or event
  nodes.

- x:

  An affiliation network.

## Value

`graph`, with the actor or event attribute added or set.

## See also

Original **igraph** functions:
[V](https://r.igraph.org/reference/V.html),
[set_vertex_attr](https://r.igraph.org/reference/set_vertex_attr.html)

Other modal queries and manipulations:
[`dualize()`](http://corybrunson.github.io/bitriad/reference/dualize.md),
[`mode_addition`](http://corybrunson.github.io/bitriad/reference/mode_addition.md),
[`mode_counts`](http://corybrunson.github.io/bitriad/reference/mode_counts.md),
[`schedule()`](http://corybrunson.github.io/bitriad/reference/schedule.md)

## Examples

``` r
data(women_clique)
print(V1(women_clique))
#> This graph was created by an old(er) igraph version.
#> ℹ Call `igraph::upgrade_graph()` on it to use with the current igraph version.
#> For now we convert it on the fly...
#> + 5/10 vertices, named, from 490393e:
#> [1] Miss A Miss B Miss C Miss D Miss E
print(V2(women_clique))
#> + 5/10 vertices, named, from 490393e:
#> [1] Bridge   Dinner   Movies   Dance    Visiting
V1(women_clique)$label <- LETTERS[1:5]
V2(women_clique)$label <- 1:5
plot(prettify_an(women_clique))
```
