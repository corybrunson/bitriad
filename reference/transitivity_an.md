# Affiliation network clustering coefficients

This function computes a given flavor of transitivity (triadic closure)
on a given affiliation network. The calculations are performed locally.
Each flavor is defined as a proportion of "wedges" that are "closed",
for suitable definitions of both terms. The function `transitivity_an`
is a shell that proceeds across actors and computes wedges using the
provided `wedgeFun`. These functions count the "wedges", and among them
the "closed" ones, centered at a given actor node in a given affiliation
network. The triads method `transitivity_an_triads` first classifies
every triad centered at each node. The appropriate formula then counts
the wedges and closed wedges at each. The method is slower for a single
flavor but can be used to produce multiple flavors with negligible
additional computational cost. The wedges method
`transitivity_an_wedges` relies on a separate "wedge function" for each
statistic. The algorithm calls the appropriate wedge function to run
over the necessary wedge centers and return a wedge count matrix, which
is returned back into `transitivity_an` for outputting.

## Usage

``` r
transitivity_an(
  graph,
  type = "global",
  wedgeFun,
  flavor,
  vids = which(!V(graph)$type),
  add.names = FALSE
)

transitivity_an_triads(graph, vids = which(!V(graph)$type), flavor)

transitivity_an_wedges(graph, vids = which(!V(graph)$type), wedgeFun)

transitivity.an(
  graph,
  type = "global",
  wedgeFun,
  flavor,
  vids = which(!V(graph)$type),
  add.names = FALSE
)

transitivity.an.triads(graph, vids = which(!V(graph)$type), flavor)

transitivity.an.wedges(graph, vids = which(!V(graph)$type), wedgeFun)

indequ_transitivity(graph, type = "global", vids = which(!V(graph)$type))

indequ.transitivity(graph, type = "global", vids = which(!V(graph)$type))

indstr_transitivity(graph, type = "global", vids = which(!V(graph)$type))

indstr.transitivity(graph, type = "global", vids = which(!V(graph)$type))

injact_transitivity(graph, type = "global", vids = which(!V(graph)$type))

injact.transitivity(graph, type = "global", vids = which(!V(graph)$type))

injequ_transitivity(graph, type = "global", vids = which(!V(graph)$type))

injequ.transitivity(graph, type = "global", vids = which(!V(graph)$type))

injstr_transitivity(graph, type = "global", vids = which(!V(graph)$type))

injstr.transitivity(graph, type = "global", vids = which(!V(graph)$type))

opsahl_transitivity(graph, type = "global", vids = which(!V(graph)$type))

opsahl.transitivity(graph, type = "global", vids = which(!V(graph)$type))

excl_transitivity(graph, type = "global", vids = which(!V(graph)$type))

excl.transitivity(graph, type = "global", vids = which(!V(graph)$type))
```

## Arguments

- graph:

  An affiliation network; see `is_an`.

- type:

  Character; the type of clustering coefficient (defaults to "global").

- wedgeFun:

  The wedge function; overrides `flavor`.

- flavor:

  The flavor of transitivity to be used; overridden by `wedgeFun`.

- vids:

  A subset of actor node ids at which to evaluate the local clustering
  coefficient.

- add.names:

  Logical; whether to label the matrix rows and columns.

- triads:

  A matrix of centered triads.

## Value

If `type` is `"global"`, the global clustering coefficient of the
network (a single numeric value); if `"local"`, the local clustering
coefficients of the actors (a numeric vector); otherwise, a 2-column
matrix, each row of which gives the number of wedges and the number of
closed wedges centered at each actor.

## See also

Other triad closure functions:
[`dynamic_triad_closure()`](http://corybrunson.github.io/bitriad/reference/dynamic_triad_closure.md),
[`project_transitivity()`](http://corybrunson.github.io/bitriad/reference/project_transitivity.md),
[`triad_closure()`](http://corybrunson.github.io/bitriad/reference/triad_closure.md),
[`triad_closure_from_census()`](http://corybrunson.github.io/bitriad/reference/triad_closure_from_census.md)
