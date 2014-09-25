bitriad
=======

This repo contains data, tools, and exposition for the **triadic analysis of two-mode networks**. It is arranged as an R package and can be installed using the "install_github" function in the [devtools](http://cran.r-project.org/web/packages/devtools/index.html) package.

## data

Sources of the network data include
* Hobson's [*The Evolution of Modern Capitalism*] [1], p. 271 (“inner”);
* Davis(, Davis), Gardner, and Gardner's [*Deep South: A Social Anthropological Study of Caste and Class*] [2], p. 148 (“davis”) and p. 209 (“women”);
* Barnes and Burkett's ["Structural Redundancy and Multiplicity in Corporate Networks"] [3] (“board”);
* The 1998 Global Information Sector, as sourced in [course notes by Knoke] [6] (“firms”);
* [Noordin Top Terrorist Network Data] [4], using meetings (“meets”) and organizations (“organ”) as events;
* Galaskiewicz's "Social organization of an urban grants economy", as reproduced in Faust's ["Centrality in affiliation networks"] [7] (“clubs”);
* Levine and Roy's "A Study of Interlocking Directorates", from [*Perspectives on Social Network Research*] [8], p. 372 (“baron”);
* Fischer's [*Paul Revere's Ride*] [5], Appendix D (“whigs”); and
* Seierstad and Opsahl's [Boards and Gender] [9], specifically the August 2009 data used in Opsahl's ["Triadic closure in two-mode networks"] [10] (“sexes”).

[1]: https://archive.org/details/evolutionofmoder00hobsuoft
[2]: http://www.amazon.com/Deep-South-Anthropological-Southern-Classics/dp/1570038155
[3]: http://www.insna.org/PDF/Connections/v30/2010_I-2_P-1-1.pdf
[4]: http://www.thearda.com/Archive/Files/Descriptions/TERRNET.asp
[5]: http://books.google.com/books/about/Paul_Revere_s_Ride.html?id=ZAvQfZFbLp4C
[6]: https://www.soc.umn.edu/~knoke/pages/Affiliations_&_Overlapping_Subgroups.doc
[7]: http://www.socsci.uci.edu/~kfaust/faust/research/articles/faust_centrality_sn_1997.pdf
[8]: http://www.sciencedirect.com/science/book/9780123525505
[9]: http://www.boardsandgender.com/data.php
[10]: http://toreopsahl.com/2011/12/21/article-triadic-closure-in-two-mode-networks-redefining-the-global-and-local-clustering-coefficients/

The folder "source_material" contains "raw" data files (pulled from the Internet where possible and otherwise constructed from primary sources) and the folder "data" contains .rda (R data) files for the affiliation networks associated with them. All graphs are bipartite; the actors and events are given "type" attributes 0 and 1, respectively, corresponding to the values FALSE and TRUE in igraph.

## tools

The folder "R" contains implementations of several triadic analysis tools. In particular these include
* twomode.triad.census, which conducts a motif survey of two-mode triads, understood to be triples of actor nodes and any events attended by at least two, and the results of which can be collapsed down to the incrementally more compact "uniformity", "cooperativity", and "simple" censuses; and
* twomode.transitivity, a shell for bipartite clustering coefficients that can be specialized to the Opsahl, "exclusive", "injseq", and "inclusive" clustering coefficients.

## expo

[This markdown file] [1] (in the "intro" folder" gives a brief overview of the tools.

[1] https://github.com/corybrunson/bitriad/blob/master/intro/bitriad-intro.md