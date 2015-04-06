#' Small inner ring of South African finance.
#'
#' An affiliation network of six multiple directors of five gold, diamond, and
#' finance companies.
#'
#' @format An undirected igraph object with nodes of logical attribute `type`;
#' directors correspond to nodes of type `FALSE`, companies to nodes of type
#' `TRUE`.
#' @source Hobson, John A. (1919) *The Evolution of Modern Capitalism*
#' \url{https://archive.org/details/evolutionofmoder00hobsuoft}
#' @name inner.circle
NULL

#' Group of women connected by event coattendance in Old City.
#'
#' A dynamic affiliation network of 18 women who attended 14 events over a 9-
#' month period.
#'
#' @format An undirected igraph object with nodes of logical attribute `type`;
#' women correspond to nodes of type `FALSE`, events to nodes of type `TRUE`.
#' Events also carry a `time` attribute equal to the number of days since the
#' previous 31 December.
#' @source Davis, Gardner, and Gardner (1941) *Deep South: A Social
#' Anthropological Study of Caste and Class*
#' \url{http://books.google.com/books?id=HGIdAAAAIAAJ}
#' @name davis.group
NULL

#' Group of women connected by event coattendance in Old City.
#'
#' A dynamic affiliation network of 5 women who participated in 5 activities.
#'
#' @format An undirected igraph object with nodes of logical attribute `type`;
#' women correspond to nodes of type `FALSE`, activities to nodes of type
#' `TRUE`.
#' @source Davis, Gardner, and Gardner (1941) *Deep South: A Social
#' Anthropological Study of Caste and Class*
#' \url{http://books.google.com/books?id=HGIdAAAAIAAJ}
#' @name davis.clique
NULL

#' Interlocks among corporate philanthropists.
#'
#' An affiliation network of 26 directors of 15 companies, taken as a subset
#' from a larger dataset.
#'
#' @format An undirected igraph object with nodes of logical attribute `type`;
#' directors correspond to nodes of type `FALSE`, companies to nodes of type
#' `TRUE`.
#' @source Wasserman & Faust (1994) *Social Network Analysis*
#' \url{http://books.google.com/books?id=CAm2DpIqRUIC}
#' @name minneapolis1970s
NULL

#' Corporate and social club interlocks among directors.
#'
#' An affiliation network of 20 directors of 24 corporations and social clubs.
#'
#' @format An undirected igraph object with nodes of logical attribute `type`;
#' directors correspond to nodes of type `FALSE`, organizations to nodes of type
#' `TRUE`.
#' @source Barnes & Burkett (2010) "Structural Redundancy and Multiplicity in
#' Corporate Networks", *Connections* **30**(2), p. 4-20.
#' \url{http://www.insna.org/PDF/Connections/v30/2010_I-2_P-1-1.pdf}
#' @name chicago1960s
NULL

#' Noordin Top meeting attendance network.
#'
#' An affiliation network of 26 individuals who attended 20 meetings
#' associated with Noordin Mohammad Top.
#'
#' @format An undirected igraph object with nodes of logical attribute `type`;
#' individuals correspond to nodes of type `FALSE`, meetings to nodes of type
#' `TRUE`.
#' @source The Association of Religious Data Archives
#' \url{http://www.thearda.com/Archive/Files/Descriptions/TERRNET.asp}
#' @name nmt.meetings
NULL

#' Noordin Top organization membership network.
#'
#' An affiliation network of 67 members of 32 organizations associated with
#' Noordin Mohammad Top.
#'
#' @format An undirected igraph object with nodes of logical attribute `type`;
#' members correspond to nodes of type `FALSE`, organizations to nodes of type
#' `TRUE`.
#' @source The Association of Religious Data Archives
#' \url{http://www.thearda.com/Archive/Files/Descriptions/TERRNET.asp}
#' @name nmt.organizations
NULL

#' Membership network of American Whigs.
#'
#' An affiliation network of 136 members of 5 American Whig organizations.
#'
#' @format An undirected igraph object with nodes of logical attribute `type`;
#' members correspond to nodes of type `FALSE`, organizations to nodes of type
#' `TRUE`.
#' @source Fischer (1994) *Paul Revere's Ride*
#' \url{https://github.com/kjhealy/revere}
#' @name whigs
NULL

