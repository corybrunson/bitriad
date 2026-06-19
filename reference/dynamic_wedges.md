# Wedge censuses and closure indicators for dynamic affiliation networks

Given a dynamic affiliation network and an actor node ID, identify all
wedges for a specified measure centered at the node and indicate whether
each is closed.

## Usage

``` r
dynamic_wedges(
  graph,
  actor,
  alcove = 0,
  wedge = 0,
  maps = 0,
  congruence = 0,
  memory = Inf,
  wedge.gap = Inf,
  close.after = 0,
  close.before = Inf
)
```

## Arguments

- graph:

  A dynamic affiliation network.

- actor:

  An actor node in `graph`.

- alcove, wedge, maps, congruence:

  Choice of alcove, wedge, maps, and congruence (see Details).

- memory:

  Numeric; minimum delay of wedge formation since would-have-been
  closing events.

- wedge.gap:

  Numeric; maximum delay between the two events of a wedge.

- close.after, close.before:

  Numeric; minimum and maximum delays after both events form a wedge for
  a third event to close it.

## Value

A two-element list consisting of (1) a 3- or 5-row integer matrix of
(representatives of) all (congruence classes of) wedges in `graph`
centered at `actor`, and (2) a logical vector indicating whether each
wedge is closed.

## Details

The `dynamic_wedges_*` functions implement wedge censuses underlying the
several measures of triad closure described below. Each function returns
a transversal of wedges from the congruence classes of wedges centered
at the index actor and indicators of whether each class is closed. The
shell function `dynamic_wedges` determines a unique measure from several
coded arguments (see below) and passes the input affiliation network to
that measure.

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

Other wedge functions:
[`indequ_wedges()`](http://corybrunson.github.io/bitriad/reference/wedges.md)
