triadic
=======

This repo contains data, tools, and exposition for the **triadic analysis of two-mode networks**.

## data

Sources of the network data include
* Hobson's [*The Evolution of Modern Capitalism*] [1], p. 271 ("H");
* Davis(, Davis), Gardner, and Gardner's [*Deep South: A Social Anthropological Study of Caste and Class*] [2], p. 148 ("DDGG1") and p. 209 ("DDGG2");
* Barnes and Burkett's ["Structural Redundancy and Multiplicity in Corporate Networks"] [3] ("BB");
* The 1998 Global Information Sector, as sourced in [course notes by Knoke] [6] ("GIS");
* [Noordin Top Terrorist Network Data] [4], using meetings ("NMT1") and organizations ("NMT2") as events;
* Galaskiewicz's "Social organization of an urban grants economy", as reproduced in Faust's ["Centrality in affiliation networks"] [7] ("G");
* Levine and Roy's "A Study of Interlocking Directorates", from [*Perspectives on Social Network Research*] [8], p. 372 ("LR");
* Fischer's [*Paul Revere's Ride*] [5], Appendix D ("F"); and
* Seierstad and Opsahl's [Boards and Gender] [9], specifically the August 2009 data used in Opsahl's ["Triadic closure in two-mode networks"] [10] ("SO").

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
* twomode.transitivity, a shell for bipartite clustering coefficients that can be specialized to the Opsahl, "inclusive", and "exclusive" clustering coefficients;
* twomode.triad.census, which conducts a motif survey of two-mode triads, understood to be triples of actor nodes and any events attended by at least two; and
* se.triad.census, an intermediate census between the cumbersome two-mode census and the 4-class simple triad census (which may be thought of as the classical triad census for directed graphs applied to an undirected simple graph by interpreting each edge as two-way).

## expo

The markdown file in the folder "intro" gives a brief overview of the tools.

## package

The repo can now be installed (or, as in the introduction, loaded without installation) as an R package.