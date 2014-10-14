bitriad
=======

This repo contains data, tools, and exposition for the **triadic analysis of two-mode networks**.

## Install

The repo is arranged as an R package and can be installed using the [devtools](http://cran.r-project.org/web/packages/devtools/index.html) package:

```r
if(!require(devtools)) {
    install.packages('devtools')
    require(devtools)
}
devtools::install_github('corybrunson/bitriad')
```

## Tools

The folder "R" contains implementations of several triadic analysis tools. In particular these include
* twomode.triad.census, which conducts a motif survey of two-mode triads, understood to be triples of actor nodes and any events attended by at least two, and the results of which can be collapsed down to the incrementally more compact "uniformity", "cooperativity", and "simple" censuses; and
* twomode.transitivity, a shell for bipartite clustering coefficients that can be specialized to the ["opsahl"] [9], "exclusive", "injseq", and "inclusive" clustering coefficients. (The Watts-Strogatz clustering coefficient is already implemented in igraph as "transitivity".)

[9]: http://toreopsahl.com/2011/12/21/article-triadic-closure-in-two-mode-networks-redefining-the-global-and-local-clustering-coefficients/
[10]: http://www.nature.com/nature/journal/v393/n6684/abs/393440a0.html

## Data

Sources of the network data include
* Hobson's [*The Evolution of Modern Capitalism*] [1], p. 271 (“hobson.inner.circle”);
* Davis(, Davis), Gardner, and Gardner's [*Deep South: A Social Anthropological Study of Caste and Class*] [2], p. 148 ("ddgg.group") and p. 209 ("ddgg.clique");
* Barnes and Burkett's ["Structural Redundancy and Multiplicity in Corporate Networks"] [3] (“barnes.burkett.corporate”);
* Galaskiewicz's "Social organization of an urban grants economy", as reproduced in Faust's ["Centrality in affiliation networks"] [7] (“galaskiewicz.urban.grants”);
* Levine and Roy's "A Study of Interlocking Directorates", from [*Perspectives on Social Network Research*] [8], p. 372 (“levine.roy.directorates”); and
* [Noordin Top Terrorist Network Data] [4], using meetings (“nordin.top.meetings”) and organizations (“nordin.top.organizations”) as events;
* Fischer's [*Paul Revere's Ride*] [5], Appendix D (“fischer.whigs”).

[1]: https://archive.org/details/evolutionofmoder00hobsuoft
[2]: http://www.amazon.com/Deep-South-Anthropological-Southern-Classics/dp/1570038155
[3]: http://www.insna.org/PDF/Connections/v30/2010_I-2_P-1-1.pdf
[4]: http://www.thearda.com/Archive/Files/Descriptions/TERRNET.asp
[5]: http://books.google.com/books/about/Paul_Revere_s_Ride.html?id=ZAvQfZFbLp4C
[7]: http://www.socsci.uci.edu/~kfaust/faust/research/articles/faust_centrality_sn_1997.pdf
[8]: http://www.sciencedirect.com/science/book/9780123525505

The folder "source_material" contains "raw" data files (pulled from the Internet where possible and otherwise constructed from primary sources) and the folder "data" contains .rda (R data) files for the affiliation networks associated with them. All graphs are bipartite; the actors and events are given "type" attributes 0 and 1, respectively, corresponding to the values FALSE and TRUE in igraph.

## Vignette

[This vignette] [11] outlines an analysis of DDGG1 and DDGG2 using the census, some clustering coefficients, and other tools.

[11]: https://github.com/corybrunson/bitriad/blob/master/intro/bitriad-intro.md