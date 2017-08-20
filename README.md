bitriad
=======

This repo constitutes an R package, and contains data and tools for the triadic analysis of affiliation networks.

## Description

The paper [*Triadic analysis of affiliation networks*](http://arxiv.org/abs/1502.07016) makes a case for adopting a batch of triad-based tools for the study of (bipartite) affiliation networks. Most of the tools used therein are included in this package, which is built mostly on the [`igraph` package](http://igraph.org/r/). No new classes have been defined yet, and all functions are written in R (rather than the more natural C/C++ used by `igraph`). Any suggestions would be welcome.

## Installation

The package is not yet on CRAN; it can be installed using the [devtools](https://github.com/hadley/devtools) package:

```r
devtools::install_github("corybrunson/bitriad")
```

If you experience any problems with the installation process (that aren't resolved by `help()` or by searching [StackExchange](http://stackexchange.com/), please let me know. I may well have left some silly error in a recent commit.

## Functionality

The package implements several tools from the paper, most importantly
* `triad_census_an`, which conducts a motif survey of affiliation network triads, understood to be triples of actor nodes and any events attended by at least two, and the results of which can be collapsed down to the incrementally more compact uniformity, structural, and simple censuses; and
* `triad_closure`, a shell for several measures of triad closure including the [Opsahl](http://toreopsahl.com/2011/12/21/article-triadic-closure-in-two-mode-networks-redefining-the-global-and-local-clustering-coefficients/), unconnected, completely connected, and exclusive clustering coefficients, including a shortcut to the [Watts-Strogatz](http://www.nature.com/nature/journal/v393/n6684/abs/393440a0.html) clustering coefficient of the unipartite projection onto actors.

## Datasets

Empirical affiliation networks from the following sources are included as datasets:
* Davis(, Davis), Gardner, Gardner(, and St Clair Drake)'s [*Deep South: A Social Anthropological Study of Caste and Class*](http://www.amazon.com/Deep-South-Anthropological-Southern-Classics/dp/1570038155), p. 148 (`women_group`) and p. 209 (`women_clique`);
* Scott and Hughes' [*The Anatomy of Scottish Capital*](http://books.google.com/books?id=59mvAwAAQBAJ), specifically Table 2, covering 1920-21 (`scotland1920s`);
* Galaskiewicz's [*Social Organization of an Urban Grants Economy*](http://books.google.com/books?id=Vd25AAAAIAAJ), specifically a subset reproduced in Faust's ["Centrality in affiliation networks"](http://www.socsci.uci.edu/~kfaust/faust/research/articles/faust_centrality_sn_1997.pdf) (`minneapolis1970s`);
* Barnes and Burkett's ["Structural Redundancy and Multiplicity in Corporate Networks"](http://www.insna.org/PDF/Connections/v30/2010_I-2_P-1-1.pdf) (`chicago1960s`);
* [Noordin Top Terrorist Network Data](http://www.thearda.com/Archive/Files/Descriptions/TERRNET.asp), using meetings (`nmt_meetings`) and organizations (`nmt_organizations`) as events;
* Fischer's [*Paul Revere's Ride*](http://books.google.com/books/about/Paul_Revere_s_Ride.html?id=ZAvQfZFbLp4C), Appendix D, as used in Han's ["The Other Ride of Paul Revere"](http://www.sscnet.ucla.edu/polisci/faculty/chwe/ps269/han.pdf) (`whigs`).

## Vignette

The vignette `southern_women` outlines an analysis of `women_clique` and `women_group` using the censuses, some clustering coefficients, and other tools.
