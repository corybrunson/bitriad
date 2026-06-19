# Project a higher-resolution triad census to a lower-resolution one

Given a triad census of any scheme, construct a triad census of a
coarser (strictly less informative) scheme.

## Usage

``` r
project_census(census, scheme = NULL, add.names = TRUE)

project.census(census, scheme = NULL, add.names = TRUE)

difference_from_full_census(census)

ftc2utc(census)

binary_from_full_census(census)

ftc2stc(census)

simple_from_full_census(census)

ftc2tc(census)

binary_from_difference_census(census)

utc2stc(census)

simple_from_difference_census(census)

utc2tc(census)

simple_from_binary_census(census)

stc2tc(census)
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

- add.names:

  Logical; whether to label the rows and columns of the output matrix.

## Details

This function inputes an affiliation network triad census of any scheme
and returns a list of triad censuses projected from it (not icluding
itself). The schemes are, in order of resolution, *full* (also called
the *affiliation network triad census* without qualification),
*difference*, *binary*, and *simple*. A final element of the output list
is the total number of triads in the affiliation network. Each summary
can be recovered from those before it, specifically by aggregating
certain matrix entries to form a smaller matrix. The helper functions
`*_from_*_census()` project a census of each scheme to one of each
coarser scheme.

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
function project_census. A fourth census, called the *uniformity triad
census* and implemented as
[unif_triad_census](http://corybrunson.github.io/bitriad/reference/triad_census.md),
is deprecated. Three-actor triad affiliation networks can be constructed
and plotted using the
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

Other triad census functions:
[`triad_census()`](http://corybrunson.github.io/bitriad/reference/triad_census.md),
[`triad_closure_from_census()`](http://corybrunson.github.io/bitriad/reference/triad_closure_from_census.md),
[`triad_tallies`](http://corybrunson.github.io/bitriad/reference/triad_tallies.md)
