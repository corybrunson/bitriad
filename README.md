triadic
=======

This repo contains data, tools, and exposition around the project "triadic analysis of two-mode networks".

## data

Sources of the network data are
* Davis, Gardner, and Gardner. [*Deep South: A Social Anthropological Study of Caste and Class*](http://www.amazon.com/Deep-South-Anthropological-Southern-Classics/dp/1570038155) (networks "women" and "davis");
* (network "board");
* (networks "meets" and "organ"); and
* Fischer. *Paul Revere's Ride* (network "whigs").

The folder "data" contains both "raw" data files (pulled from the Internet where possible and otherwise constructed from primary sources) and GML files for the affiliation networks associated with them. All graphs are bipartite; the actors and events are given "type" attributes 0 and 1, respectively, corresponding to the values FALSE and TRUE in igraph.

## tools

The folder "functions" contains implementations of several triadic analysis tools:
* "twomode.transitivity.R" contains a shell for bipartite clustering coefficients with specializations to the Opsahl, inclusive, and exclusive clustering coefficients;
* "partition.bijections.R" contains some helpful combinatorial bijections;
* "twomode.triad.census.R" contains two implementations of the title tool plus several functions that recover lower-order information from it (e.g. global clustering coefficients and a structural-equivalence triad census).

## expo

