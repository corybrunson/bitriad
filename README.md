triadic
=======

This repo contains data, tools, and exposition for the **triadic analysis of two-mode networks**.

## data

Sources of the network data include
* Davis, Gardner, and Gardner's [*Deep South: A Social Anthropological Study of Caste and Class*](http://www.amazon.com/Deep-South-Anthropological-Southern-Classics/dp/1570038155) ("davis" and "davis2");
* Barnes and Burkett's ["Structural Redundancy and Multiplicity in Corporate Networks"](http://www.insna.org/PDF/Connections/v30/2010_I-2_P-1-1.pdf) ("directors");
* [Noordin Top Terrorist Network Data](http://www.thearda.com/Archive/Files/Descriptions/TERRNET.asp) ("meetings" and "organizations"); and
* Fischer's [*Paul Revere's Ride*](http://books.google.com/books/about/Paul_Revere_s_Ride.html?id=ZAvQfZFbLp4C) ("whigs").

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