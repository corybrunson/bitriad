# Triadic closure for dynamic affiliation networks

Given an affiliation network with time-stamped events, compute the
proportion of centered triples at which an open wedge exists at some
time that is closed at a later time.

## Usage

``` r
dynamic_triad_closure(
  graph,
  actors = V(graph)[V(graph)$type == FALSE],
  type = "global",
  ...,
  measure = NULL
)

dynamic_triad_closure_an(
  graph,
  actors = V(graph)[V(graph)$type == FALSE],
  type = "global",
  ...,
  measure = NULL
)

dynamic_transitivity_an(
  graph,
  actors = V(graph)[V(graph)$type == FALSE],
  type = "global",
  ...,
  measure = NULL
)

dyn.transitivity.an(
  graph,
  actors = V(graph)[V(graph)$type == FALSE],
  type = "global",
  ...,
  measure = NULL
)

dynamic_triad_closure_projection(graph, memory = Inf, type = "global")
```

## Arguments

- graph:

  An affiliation network with time-stamped events.

- actors:

  A vector of actor nodes in `graph`.

- type:

  The type of statistic, matched to `"global"`, `"local"`, or `"raw"`.

- ...:

  Additional parameters passed to specific functions.

- measure:

  Character; the measure of triad closure, used as the suffix `*` to
  `triad_closure_*`. Matched to `"classical"` (also `"watts_strogatz"`),
  `"twomode"` (also `"opsahl"`), `"unconnected"` (also
  `"liebig_rao_0"`), `"completely_connected"` (also `"liebig_rao_3"`),
  `"exclusive"`, or `"projection"`.

- memory:

  Numeric; minimum delay of wedge formation since would-have-been
  closing events.

## Value

A numeric vector of the same length as `actors`.

## See also

Other triad closure functions:
[`project_transitivity()`](http://corybrunson.github.io/bitriad/reference/project_transitivity.md),
[`transitivity_an()`](http://corybrunson.github.io/bitriad/reference/transitivity_an.md),
[`triad_closure()`](http://corybrunson.github.io/bitriad/reference/triad_closure.md),
[`triad_closure_from_census()`](http://corybrunson.github.io/bitriad/reference/triad_closure_from_census.md)

## Examples

``` r
data(women_group)
dynamic_triad_closure(women_group)
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
#> [1] 0.5464481
cbind(
  transitivity(actor_projection(women_group), type = "local"),
  triad_closure_opsahl(women_group, type = "local"),
  triad_closure_exclusive(women_group, type = "local"),
  dynamic_triad_closure_projection(women_group, type = "local"),
  dynamic_triad_closure(women_group, type = "local")
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
#>                [,1]      [,2]      [,3]      [,4]      [,5]
#> Evelyn    0.8970588 0.7666667 0.4477612 0.5757576 0.4496124
#> Laura     0.9619048 0.8421751 0.4871795 0.6923077 0.6052632
#> Theresa   0.8970588 0.7523437 0.1445783 0.6500000 0.5144509
#> Brenda    0.9619048 0.8387909 0.4500000 0.6923077 0.4871795
#> Charlotte 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000
#> Frances   0.9619048 0.8690476 0.7777778 0.0000000 0.0000000
#> Eleanor   0.9619048 0.7959184 0.5312500 0.6923077 0.5652174
#> Pearl     0.9333333 0.6462585 0.4666667 0.6363636 0.4666667
#> Ruth      0.8970588 0.6702509 0.3283582 0.6500000 0.5000000
#> Verne     0.8970588 0.6740891 0.3928571 0.5757576 0.5185185
#> Myra      0.9333333 0.7138810 0.5555556 0.2727273 0.4285714
#> Katherine 0.9333333 0.7695560 0.5357143 0.2727273 0.4285714
#> Sylvia    0.8970588 0.7461929 0.3000000 0.5757576 0.5714286
#> Nora      0.8970588 0.8379501 0.6631579 0.7254902 0.7207792
#> Helen     0.8970588 0.8159204 0.6610169 0.6111111 0.5714286
#> Dorothy   0.9333333 0.5407407 0.4666667 0.0000000 0.0000000
#> Olivia    1.0000000 0.5806452 1.0000000 1.0000000 1.0000000
#> Flora     1.0000000 0.5806452 1.0000000 1.0000000 1.0000000
```
