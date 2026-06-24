# Global triad closure from a triad census

Given a triad census of a suitable scheme, calculate a global measure of
triad closure for the associated affiliation network.

## Usage

``` r
triad_closure_from_census(
  census,
  scheme = NULL,
  alcove = 0,
  wedge = 0,
  maps = 0,
  congruence = 0,
  measure = NULL,
  open.fun = NULL,
  closed.fun = NULL,
  counts = FALSE
)

triad_closure_from_simple_census(
  census,
  alcove = 0,
  wedge = 0,
  maps = 0,
  congruence = 0,
  open.fun = NULL,
  closed.fun = NULL,
  counts = FALSE
)

triad_closure_from_binary_census(
  census,
  alcove = 0,
  wedge = 0,
  maps = 0,
  congruence = 0,
  open.fun = NULL,
  closed.fun = NULL,
  counts = FALSE
)

triad_closure_from_difference_census(
  census,
  alcove = 0,
  wedge = 0,
  maps = 0,
  congruence = 0,
  open.fun = NULL,
  closed.fun = NULL,
  counts = FALSE
)

triad_closure_from_full_census(
  census,
  alcove = 0,
  wedge = 0,
  maps = 0,
  congruence = 0,
  open.fun = NULL,
  closed.fun = NULL,
  counts = FALSE
)

wedges_from_full_census(census, open.fun, closed.fun)

wedges_from_census(...)

wedgecount_census(...)

wedgecount.census(...)

triad_closure_from_census_original(
  census,
  scheme = NULL,
  alcove = 0,
  wedge = 0,
  maps = 0,
  congruence = 0,
  measure,
  open.fun,
  closed.fun,
  counts = FALSE
)

transitivity_from_census(...)

transitivity.census(...)
```

## Arguments

- census:

  Numeric matrix or vector; an affiliation network triad census. It is
  treated as binary or simple if its dimensons are 4-by-2 or 4-by-1,
  respectively, unless otherwise specified by `scheme`; otherwise it is
  treated as full.

- scheme:

  Character; the type of triad census provided, matched to `"full"`,
  `"difference"` (also `"uniformity"`), `"binary"` (also
  `"structural"`), or `"simple"`.

- alcove, wedge, maps, congruence:

  Choice of alcove, wedge, maps, and congruence (see Details).

- measure:

  Character; the measure of triad closure (matched to "classical",
  "watts_strogatz", "twomode", "opsahl", "unconnected", "liebig_rao_0",
  "completely_connected", "liebig_rao_3", "exclusive", "allact",
  "indequ", "indstr", "injact", "injequ", or "injstr"). Overrides
  `alcove`, `wedge`, `maps`, and `congruence`.

- open.fun, closed.fun:

  Functions to calculate the open and closed wedge count for a triad
  (when `scheme` is `"full"`) or a triad census (otherwise), in order to
  calculate a custom measure of triad closure. Override `measure`.

- counts:

  Logical; whether to return open and closed wedge counts instead of the
  quotient.

- ...:

  Arguments passed from deprecated functions to their replacements.

## Value

Output equivalent to that of
[`triad_closure()`](http://corybrunson.github.io/bitriad/reference/triad_closure.md).

## Details

Each global measure of triad closure can be recovered from the full
triad census, and some can be recovered from smaller censuses. This
function verifies that a given census is sufficient to recover a given
measure of triad closure and, if it is, returns its value.

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
  [triad_census_an](http://corybrunson.github.io/bitriad/reference/triad_census.md)
  (called from `triad_census` when the `graph` argument is an
  `affiliation_network`) defaults to this census.

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
  the *uniformity triad census*, implemented as
  [triad_census_difference](http://corybrunson.github.io/bitriad/reference/triad_census.md),
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
  [triad_census_binary](http://corybrunson.github.io/bitriad/reference/triad_census.md),
  records the number of triads in each congruence class.

- The *simple triad census* is the 4-entry triad census on a traditional
  (non-affiliation) network indicating the number of triads of each
  isomorphism class, namely whether the triad contains zero, one, two,
  or three links. The function
  [simple_triad_census](http://corybrunson.github.io/bitriad/reference/triad_census.md)
  computes the classical (undirected) triad census for an undirected
  traditional network, or for the actor projection of an affiliation
  network (if provided), using
  [triad_census](http://corybrunson.github.io/bitriad/reference/triad_census.md);
  if the result doesn't make sense (i.e., the sum of the entries is not
  the number of triples of nodes), then it instead uses its own, much
  slower method.

Each of these censuses can be projected from the previous using the
function
[project_census](http://corybrunson.github.io/bitriad/reference/project_census.md).
A fourth census, called the *uniformity triad census* and implemented as
[unif_triad_census](http://corybrunson.github.io/bitriad/reference/triad_census.md),
is deprecated. Three-actor triad affiliation networks can be constructed
and plotted using the
[triad](http://corybrunson.github.io/bitriad/reference/triad.md)
functions.

The default method for the two affiliation network–specific triad
censuses is adapted from the algorithm of Batagelj and Mrvar (2001) for
calculating the classical triad census for a directed graph.

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

Kreher, D.L., & Stinson, D.R. (1999). Combinatorial algorithms:
generation, enumeration, and search. *SIGACT News*, 30(1), 33–35.

Batagelj, V., & Mrvar, A. (2001). A subquadratic triad census algorithm
for large sparse networks with small maximum degree. *Social Networks*,
23(3), 237–243.

Brunson, J.C. (2015). Triadic analysis of affiliation networks. *Network
Science*, 3(4), 480–508.

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

Other triad census functions:
[`project_census()`](http://corybrunson.github.io/bitriad/reference/project_census.md),
[`triad_census()`](http://corybrunson.github.io/bitriad/reference/triad_census.md),
[`triad_tallies`](http://corybrunson.github.io/bitriad/reference/triad_tallies.md)

Other triad closure functions:
[`dynamic_triad_closure()`](http://corybrunson.github.io/bitriad/reference/dynamic_triad_closure.md),
[`project_transitivity()`](http://corybrunson.github.io/bitriad/reference/project_transitivity.md),
[`transitivity_an()`](http://corybrunson.github.io/bitriad/reference/transitivity_an.md),
[`triad_closure()`](http://corybrunson.github.io/bitriad/reference/triad_closure.md)
