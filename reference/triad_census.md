# Triad census for affiliation networks

Given an affiliation network, tally all actor triads by isomorphism or
other congruence class.

## Usage

``` r
triad_census(graph, ..., add.names = TRUE)

triad_census_an(
  graph,
  scheme = "full",
  method = "batagelj_mrvar",
  ...,
  add.names = TRUE
)

triad.census.an(...)

triad_census_full(graph, method = "batagelj_mrvar", ..., add.names = TRUE)

triad_census_full_batagelj_mrvar(graph, use.integer = FALSE)

triad_census_full_projection(graph, verbose = FALSE)

triad_census_difference(
  graph,
  method = "batagelj_mrvar",
  ...,
  add.names = TRUE
)

triad_census_difference_batagelj_mrvar(graph, use.integer = FALSE)

triad_census_difference_projection(graph)

unif_triad_census(graph)

unif.triad.census(graph)

triad_census_binary(graph, method = "batagelj_mrvar", ..., add.names = TRUE)

triad_census_binary_batagelj_mrvar(graph, use.integer = FALSE)

triad_census_binary_projection(graph, verbose = FALSE)

str_triad_census(graph)

structural.triad.census(graph)

simple_triad_census(graph, add.names = TRUE)

simple.triad.census(graph, add.names = TRUE)
```

## Arguments

- graph:

  An **igraph** object, usually an affiliation network.

- ...:

  Additional arguments (currently `use.integer` and `verbose`) passed to
  the `method` function.

- add.names:

  Logical; whether to label the rows and columns of the output matrix.

- scheme:

  Character; the type of triad census to calculate, matched to `"full"`,
  `"difference"` (also `"uniformity"`), `"binary"` (also
  `"structural"`), or `"simple"`.

- method:

  Character; the triad census method to use. Currently only
  `"batagelj_mrvar"` is implemented. `"projection"` calls an inefficient
  but reliable implementation in R from the first package version that
  invokes the simple_triad_census of the
  [actor_projection](http://corybrunson.github.io/bitriad/reference/mode_projection.md)
  of `graph`.

- use.integer:

  Logical; whether to use the `IntegerMatrix` class in **Rcpp** rather
  than the default `NumericMatrix`.

- verbose:

  Logical; whether to display progress bars.

## Value

A matrix counts of triad congruence classes, with row indices reflecting
pairwise exclusive events and column indices reflecting triadwise
events.

## Details

The `triad_census_*` functions implement the several triad censuses
described below. Each census is based on a congruence relation among the
triads in an affiliation network, and each function returns a matrix
(or, in the "simple" case, a vector) recording the number of triads in
each congruence class.

The function `triad_census` supercedes triad_census but calls in case
`graph` is not an affiliation network.

## Triad censuses

Three triad censuses are implemented for affiliation networks:

- The *full triad census* (Brunson, 2015) records the number of triads
  of each isomorphism class. The classes are indexed by a partition,
  \\\lambda=(\lambda_1\leq\lambda_2\leq\lambda_3)\\, indicating the
  number of events attended by both actors in each pair but not the
  third, and a positive integer, \\w\\, indicating the number of events
  attended by all three actors. The isomorphism classes are organized
  into a matrix with rows indexed by \\\lambda\\ and columns indexed by
  \\w\\, with the partitions \\\lambda\\ ordered according to the
  *revolving door ordering* (Kreher & Stinson, 1999). The main function
  triad_census_an (called from `triad_census` when the `graph` argument
  is an `affiliation_network`) defaults to this census.

- For the analysis of sparse affiliation networks, the full triad census
  may be less useful than information on whether the extent of
  connectivity through co-attended events differs between each pair of
  actors. In order to summarize this information, a coarser triad census
  can be conducted on classes of triads based on the following
  congruence relation: Using the indices \\\lambda=(x\ge y\ge z)\\ and
  \\w\\ above, note that the numbers of shared events for each pair and
  for the triad are \\x+w\ge y+w\ge z+w\ge w\ge 0\\. Consider two triads
  congruent if the same subset of these weak inequalities are strictly
  satisfied. The resulting *difference triad census*, previously called
  the *uniformity triad census*, implemented as triad_census_difference,
  is organized into a \\8\times 2\\ matrix with the strictness of the
  first three inequalities determining the row and that of the last
  inequality determining the column.

- A still coarser congruence relation can be used to tally how many are
  connected by at least one event in each distinct way. This relation
  considers two triads congruent if each corresponding pair of actors
  both attended or did not attend at least one event not attended by the
  third, and if the corresponding triads both attended or did not attend
  at least one event together. The *binary triad census* (Brunson, 2015;
  therein called the *structural triad census*), implemented as
  triad_census_binary, records the number of triads in each congruence
  class.

- The *simple triad census* is the 4-entry triad census on a traditional
  (non-affiliation) network indicating the number of triads of each
  isomorphism class, namely whether the triad contains zero, one, two,
  or three links. The function simple_triad_census computes the
  classical (undirected) triad census for an undirected traditional
  network, or for the actor projection of an affiliation network (if
  provided), using triad_census; if the result doesn't make sense (i.e.,
  the sum of the entries is not the number of triples of nodes), then it
  instead uses its own, much slower method.

Each of these censuses can be projected from the previous using the
function
[project_census](http://corybrunson.github.io/bitriad/reference/project_census.md).
A fourth census, called the *uniformity triad census* and implemented as
unif_triad_census, is deprecated. Three-actor triad affiliation networks
can be constructed and plotted using the
[triad](http://corybrunson.github.io/bitriad/reference/triad.md)
functions.

The default method for the two affiliation network–specific triad
censuses is adapted from the algorithm of Batagelj and Mrvar (2001) for
calculating the classical triad census for a directed graph.

## References

Kreher, D.L., & Stinson, D.R. (1999). Combinatorial algorithms:
generation, enumeration, and search. *SIGACT News*, 30(1), 33–35.

Batagelj, V., & Mrvar, A. (2001). A subquadratic triad census algorithm
for large sparse networks with small maximum degree. *Social Networks*,
23(3), 237–243.

Brunson, J.C. (2015). Triadic analysis of affiliation networks. *Network
Science*, 3(4), 480–508.

## See also

Original **igraph** functions: triad_census

Other triad census functions:
[`project_census()`](http://corybrunson.github.io/bitriad/reference/project_census.md),
[`triad_closure_from_census()`](http://corybrunson.github.io/bitriad/reference/triad_closure_from_census.md),
[`triad_tallies`](http://corybrunson.github.io/bitriad/reference/triad_tallies.md)

## Examples

``` r
data(women_clique)
(tc <- triad_census(women_clique, add.names = TRUE))
#> This graph was created by an old(er) igraph version.
#> ℹ Call `igraph::upgrade_graph()` on it to use with the current igraph version.
#> For now we convert it on the fly...
#>         0 1
#> (0,0,0) 0 0
#> (1,0,0) 0 1
#> (1,1,0) 0 3
#> (1,1,1) 1 0
#> (2,0,0) 0 0
#> (2,1,0) 3 0
#> (2,1,1) 2 0
#> (2,2,0) 0 0
#> (2,2,1) 0 0
#> (2,2,2) 0 0
sum(tc) == choose(vcount(actor_projection(women_clique)), 3)
#> [1] TRUE
```
