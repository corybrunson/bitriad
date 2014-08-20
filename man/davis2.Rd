\name{davis2}
\alias{davis2}
\docType{data}
\title{Davis et al southern (black) women}
\description{The data consist of five events attended by five black women in a rural town in the 1930s deep south, forming an affiliation network. They are stored as a bipartite graph with women of type FALSE and events of type TRUE.}
\usage{data(davis2)}
\format{
  The format is:
IGRAPH UN-B 10 14 -- 
+ attr: type (v/l), name (v/c)
+ edges (vertex names):
Miss A -- Bridge, Movies, Dance
Miss B -- Movies, Dance
Miss C -- Bridge, Dinner, Visiting
Miss D -- Bridge, Dinner, Movies
Miss E -- Dinner, Dance, Visiting
Bridge -- Miss A, Miss C, Miss D
Dinner -- Miss C, Miss D, Miss E
Movies -- Miss A, Miss B, Miss D
Dance -- Miss A, Miss B, Miss E
Visiting -- Miss C, Miss E
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(davis2)
## maybe str(davis2) ; plot(davis2) ...
}
\keyword{datasets}
