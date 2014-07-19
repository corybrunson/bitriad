triadic
=======

This repo contains data, tools, and exposition for the **triadic analysis of two-mode networks**.

## data

Sources of the network data are
* Davis, Gardner, and Gardner's [*Deep South: A Social Anthropological Study of Caste and Class*](http://www.amazon.com/Deep-South-Anthropological-Southern-Classics/dp/1570038155) ("women" and "davis");
* Barnes and Burkett's ["Structural Redundancy and Multiplicity in Corporate Networks"](http://www.insna.org/PDF/Connections/v30/2010_I-2_P-1-1.pdf) ("board");
* [Noordin Top Terrorist Network Data](http://www.thearda.com/Archive/Files/Descriptions/TERRNET.asp) ("meets" and "organ"); and
* Fischer's [*Paul Revere's Ride*](http://books.google.com/books/about/Paul_Revere_s_Ride.html?id=ZAvQfZFbLp4C) ("whigs").

The folder "data" contains both "raw" data files (pulled from the Internet where possible and otherwise constructed from primary sources) and GML files for the affiliation networks associated with them. All graphs are bipartite; the actors and events are given "type" attributes 0 and 1, respectively, corresponding to the values FALSE and TRUE in igraph.

## tools

The folder "functions" contains implementations of several triadic analysis tools:
* "twomode.transitivity.R" contains a shell for bipartite clustering coefficients with specializations to the Opsahl, inclusive, and exclusive clustering coefficients;
* "partition.bijections.R" contains some helpful combinatorial bijections;
* "twomode.triad.census.R" contains two implementations of the title tool plus several functions that recover lower-order information from it (e.g. global clustering coefficients and a structural-equivalence triad census).

## expo

The markdown file in the folder "intro" gives a brief overview of the tools.
