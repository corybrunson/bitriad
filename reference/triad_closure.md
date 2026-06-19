# Triad closure for affiliation networks

Given an affiliation network and a vector of actor node IDs, calculate a
specified measure of triad closure centered at the nodes.

## Usage

``` r
triad_closure(graph, ...)

triad_closure_an(graph, method = "wedges", ...)

triad_closure_via_triads(
  graph,
  actors = V(graph)[V(graph)$type == FALSE],
  type = "global",
  ...
)

triad_closure_from_centered_triads(
  triad_list,
  type = "global",
  ...,
  measure = NULL,
  triads.fun = NULL
)

triad_closure_via_wedges(
  graph,
  actors = V(graph)[V(graph)$type == FALSE],
  type = "global",
  ...,
  measure = NULL,
  wedges.fun = NULL
)

triad_closure_watts_strogatz(
  graph,
  actors = V(graph)[V(graph)$type == FALSE],
  type = "global"
)

triad_closure_classical(
  graph,
  actors = V(graph)[V(graph)$type == FALSE],
  type = "global"
)

triad_closure_opsahl(
  graph,
  actors = V(graph)[V(graph)$type == FALSE],
  type = "global"
)

triad_closure_twomode(
  graph,
  actors = V(graph)[V(graph)$type == FALSE],
  type = "global"
)

triad_closure_liebig_rao_0(
  graph,
  actors = V(graph)[V(graph)$type == FALSE],
  type = "global"
)

triad_closure_unconnected(
  graph,
  actors = V(graph)[V(graph)$type == FALSE],
  type = "global"
)

triad_closure_liebig_rao_3(
  graph,
  actors = V(graph)[V(graph)$type == FALSE],
  type = "global"
)

triad_closure_completely_connected(
  graph,
  actors = V(graph)[V(graph)$type == FALSE],
  type = "global"
)

triad_closure_exclusive(
  graph,
  actors = V(graph)[V(graph)$type == FALSE],
  type = "global"
)

triad_closure_projection(
  graph,
  actors = V(graph)[V(graph)$type == FALSE],
  type = "global"
)
```

## Arguments

- graph:

  An affiliation network.

- ...:

  Measure specifications passed to
  [wedges](http://corybrunson.github.io/bitriad/reference/wedges.md).

- method:

  Character; for a given `measure`, whether to use the measure-specific
  wedge census (`"wedges"`) or the measure-specific calculation on the
  centered triad census (`"triads"`).

- actors:

  A vector of actor nodes in `graph`.

- type:

  The type of statistic, matched to `"global"`, `"local"`, or `"raw"`.

- triad_list:

  A list of triad isomorphism classes in matrix format, as produced by
  [centered_triads](http://corybrunson.github.io/bitriad/reference/wedges.md).

- measure:

  Character; the measure of triad closure, used as the suffix `*` to
  `triad_closure_*` Matched to `"classical"` (also `"watts_strogatz"`),
  `"twomode"` (also `"opsahl"`), `"unconnected"` (also
  `"liebig_rao_0"`), `"completely_connected"` (also `"liebig_rao_3"`),
  or `"exclusive"`.

- triads.fun:

  A custom triad closure calculation. It must accept a vector of
  *centered* triad isomorphism classes, encoded as vectors `w`, `x`,
  `y`, and `z`, and return a 2-row integer matrix recording the number
  of wedges of the desired measure centered at the second actor, and
  involving the other two actors, of each triad.

- wedges.fun:

  A custom wedge census function. It must accept an affiliation network
  `graph` and a single actor node ID `actor` and may have any additional
  parameters. It must return a named list with values `wedges` a numeric
  matrix of node IDs whose columns record the wedges centered at `actor`
  and `closed` a logical vector recording whether each wedge is closed.
  Overrides `measure`.

## Value

If `type` is `"global"`, the global statistic for `graph`; if `"local"`,
the local statistics for `actors`; if `"raw"`, a 2-column matrix, each
row of which gives the number of wedges and of closed wedges centered at
`actors`.

## Details

The `triad_closure_*` functions implement the several measures of triad
closure described below. Each function returns a single global
statistic, a vector of local statistics, or a matrix of local
denominators and numerators from which the global and local statistics
can be recovered.

The function `triad_closure_projection` recapitulates
triad_closure_watts_strogatz by invoking the
[bipartite_projection](https://r.igraph.org/reference/bipartite_projection.html)
and [transitivity](https://r.igraph.org/reference/transitivity.html)
functions in **igraph**.

## Measures of triad closure

Each measure of triad closure is defined as the proportion of wedges
that are closed, where a *wedge* is the image of a specified two-event
triad \\W\\ under a specified subcategory of graph maps \\C\\ subject to
a specified congruence relation \\~\\, and where a wedge is *closed* if
it is the image of such a map that factors through a canonical inclusion
of \\W\\ to a specified self-dual three-event triad \\X\\.

The alcove, wedge, maps, and congruence can be specified by numerical
codes as follows (no plans exist to implement more measures than these):

- `alcove`:

  - `0`: \\T\_{(1,1,1),0}\\

  - `1`: \\T\_{(1,1,0),1}\\ (**not yet implemented**)

  - `2`: \\T\_{(1,0,0),2}\\ (**not yet implemented**)

  - `3`: \\T\_{(0,0,0),3}\\ (**not yet implemented**)

- `wedge`:

  - `0`: \\T\_{(1,1,0),0}\\

  - `1`: \\T\_{(1,0,0),1}\\ (**not yet implemented**)

  - `2`: \\T\_{(0,0,0),2}\\ (**not yet implemented**)

- `maps`:

  - `0`: all graph maps (injective on actors)

  - `1`: injective graph maps

  - `2`: induced injective graph maps

- `congruence`:

  - `0`: same actor and event images (equivalence)

  - `1`: same actor images, structurally equivalent event images

  - `2`: same actor images

Some specifications correspond to statistics of especial interest:

- `0,0,0,2`: the classical clustering coefficient (Watts & Strogatz,
  1998), evaluated on the unipartite actor projection

- `0,0,1,0`: the two-mode clustering coefficient (Opsahl, 2013)

- `0,0,2,0`: the unconnected clustering coefficient (Liebig & Rao, 2014)

- `3,2,2,0`: the completely connected clustering coefficient (Liebig &
  Rao, 2014) (**not yet implemented**)

- `0,0,2,1`: the exclusive clustering coefficient (Brunson, 2015)

- `0,0,2,2`: the exclusive clustering coefficient

See Brunson (2015) for a general definition and the aforecited
references for discussions of each statistic.

## References

Watts, D.J., & Strogatz, S.H. (1998). Collective dynamics of
"small-world" networks. *Nature*, 393(6684), 440–442.

Opsahl, T. (2013). Triadic closure in two-mode networks: Redefining the
global and local clustering coefficients. *Social Networks*, 35(2),
159–167. Special Issue on Advances in Two-mode Social Networks.

Liebig, J., & Rao, A. (2014). Identifying influential nodes in bipartite
networks using the clustering coefficient. Pages 323–330 of:
*Proceedings of the tenth international conference on signal-image
technology and internet-based systems*.

Brunson, J.C. (2015). Triadic analysis of affiliation networks. *Network
Science*, 3(4), 480–508.

## See also

Original **igraph** functions:
[transitivity](https://r.igraph.org/reference/transitivity.html)

Other triad closure functions:
[`dynamic_triad_closure()`](http://corybrunson.github.io/bitriad/reference/dynamic_triad_closure.md),
[`project_transitivity()`](http://corybrunson.github.io/bitriad/reference/project_transitivity.md),
[`transitivity_an()`](http://corybrunson.github.io/bitriad/reference/transitivity_an.md),
[`triad_closure_from_census()`](http://corybrunson.github.io/bitriad/reference/triad_closure_from_census.md)

## Examples

``` r
data(women_clique)
mapply(
  triad_closure,
  measure = c("classical", "twomode", "unconnected", "exclusive"),
  MoreArgs = list(graph = women_clique, type = "local")
)
#> This graph was created by an old(er) igraph version.
#> ℹ Call `igraph::upgrade_graph()` on it to use with the current igraph version.
#> For now we convert it on the fly...
#>      classical   twomode unconnected exclusive
#> [1,] 0.8333333 0.5000000   0.5000000      0.50
#> [2,] 1.0000000 0.6666667   1.0000000      1.00
#> [3,] 1.0000000 0.6666667   0.6666667      0.50
#> [4,] 0.8333333 0.6000000   0.5000000      0.50
#> [5,] 0.8333333 0.7142857   0.6666667      0.75
data(women_group)
cbind(
  triad_closure_watts_strogatz(women_group, type = "local"),
  triad_closure_opsahl(women_group, type = "local"),
  triad_closure_liebig_rao_0(women_group, type = "local"),
  triad_closure_exclusive(women_group, type = "local")
)
#> This graph was created by an old(er) igraph version.
#> ℹ Call `igraph::upgrade_graph()` on it to use with the current igraph version.
#> For now we convert it on the fly...
#>            [,1]      [,2]       [,3]      [,4]
#>  [1,] 0.8970588 0.7666667 0.39574468 0.4477612
#>  [2,] 0.9619048 0.8421751 0.44680851 0.4871795
#>  [3,] 0.8970588 0.7523437 0.06188925 0.1445783
#>  [4,] 0.9619048 0.8387909 0.34545455 0.4500000
#>  [5,] 1.0000000 1.0000000 1.00000000 1.0000000
#>  [6,] 0.9619048 0.8690476 0.66666667 0.7777778
#>  [7,] 0.9619048 0.7959184 0.50943396 0.5312500
#>  [8,] 0.9333333 0.6462585 0.40740741 0.4666667
#>  [9,] 0.8970588 0.6702509 0.28688525 0.3283582
#> [10,] 0.8970588 0.6740891 0.37777778 0.3928571
#> [11,] 0.9333333 0.7138810 0.67346939 0.5555556
#> [12,] 0.9333333 0.7695560 0.72602740 0.5357143
#> [13,] 0.8970588 0.7461929 0.33950617 0.3000000
#> [14,] 0.8970588 0.8379501 0.71854305 0.6631579
#> [15,] 0.8970588 0.8159204 0.71428571 0.6610169
#> [16,] 0.9333333 0.5407407 0.46666667 0.4666667
#> [17,] 1.0000000 0.5806452 1.00000000 1.0000000
#> [18,] 1.0000000 0.5806452 1.00000000 1.0000000
```
