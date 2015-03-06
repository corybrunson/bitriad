bitriad
=======

This repo contains data, tools, and exposition for the **triadic analysis of affiliation networks**.

## Description

The paper [*Triadic analysis of affiliation networks*] [6] makes a case for adopting a batch of triad-centric tools for the study of (bipartite) affiliation networks; the examples invoked are affiliation networks. Most of the tools are included in the `bitriad` package. The [igraph package](http://igraph.org/r/) provides the class of graphs and the basic suite of graph-theoretic tools, which serve as a foundation. No new classes have been defined, and all functions are written in R rather than called from, say, C. Any suggestions would be welcome.

[6]: http://arxiv.org/abs/1502.07016

## Install

The repo is arranged as an R package and can be installed using the [devtools](http://cran.r-project.org/web/packages/devtools/index.html) package:

```r
if(!require(devtools)) {
    install.packages('devtools')
    if(require(devtools))
        install_github('corybrunson/bitriad')
    else
        stop('Fail.')
}
```

If you experience any problems with the installation process (that aren't resolved by `?` and searching, e.g., [StackExchange] [3], please let me know.

[3]: http://stackexchange.com/

## Tools

The folder `R` contains implementations of several triadic analysis tools. In particular these include
* an.triad.census, which conducts a motif survey of affiliation network triads, understood to be triples of actor nodes and any events attended by at least two, and the results of which can be collapsed down to the incrementally more compact uniformity, structural, and simple censuses; and
* an.transitivity, a shell for bipartite clustering coefficients that can be specialized to the [Opsahl] [9], exclusive, and several other clustering coefficients, including a shortcut to the Watts-Strogatz clustering coefficient of the projection onto actors.

[9]: http://toreopsahl.com/2011/12/21/article-triadic-closure-in-two-mode-networks-redefining-the-global-and-local-clustering-coefficients/
[10]: http://www.nature.com/nature/journal/v393/n6684/abs/393440a0.html

## Data

Sources of the network data include
* Hobson's [*The Evolution of Modern Capitalism*] [1], p. 271 (`hobson.inner.circle`);
* Davis(, Davis), Gardner, Gardner(, and St Clair Drake)'s [*Deep South: A Social Anthropological Study of Caste and Class*] [2], p. 148 (`ddggs.group`) and p. 209 (`ddggs.clique`);
* Scott and Hughes' [*The Anatomy of Scottish Capital*] [12], specifically Table 2, covering 1920-21 (`scott.hughes.capital`);
* Galaskiewicz's [*Social Organization of an Urban Grants Economy*] [8], specifically a subset reproduced in Faust's ["Centrality in affiliation networks"] [7] (`galaskiewicz.urban.grants`);
* Barnes and Burkett's ["Structural Redundancy and Multiplicity in Corporate Networks"] [3] (`barnes.burkett.corporate`);
* [Noordin Top Terrorist Network Data] [4], using meetings (“nordin.top.meetings”) and organizations (`nordin.top.organizations`) as events;
* Fischer's [*Paul Revere's Ride*] [5], Appendix D, as used in Han's ["The Other Ride of Paul Revere"] [13] (`fischer.han.whigs`).

[1]: https://archive.org/details/evolutionofmoder00hobsuoft
[2]: http://www.amazon.com/Deep-South-Anthropological-Southern-Classics/dp/1570038155
[3]: http://www.insna.org/PDF/Connections/v30/2010_I-2_P-1-1.pdf
[4]: http://www.thearda.com/Archive/Files/Descriptions/TERRNET.asp
[5]: http://books.google.com/books/about/Paul_Revere_s_Ride.html?id=ZAvQfZFbLp4C
[7]: http://www.socsci.uci.edu/~kfaust/faust/research/articles/faust_centrality_sn_1997.pdf
[8]: http://books.google.com/books?id=Vd25AAAAIAAJ
[12]: http://books.google.com/books?id=59mvAwAAQBAJ
[13]: http://www.sscnet.ucla.edu/polisci/faculty/chwe/ps269/han.pdf

The folder `data` contains .rda (R data) files for the affiliation networks associated with them. All graphs are bipartite; the actors and events are given `type` attributes `FALSE` and `TRUE`, respectively.

## Vignette

[This vignette] [11] outlines an analysis of `ddggs.clique` and `ddggs.group` using the censuses, some clustering coefficients, and other tools.

[11]: https://github.com/corybrunson/bitriad/blob/master/vignettes/southern_women.md