\name{finance}
\alias{finance}
\docType{data}
\title{Hobson's inner ring of South African finance}
\description{The data consist of the directorate of five representative companies from the 1910s South African finance sector, forming an affiliation network. They are stored as a bipartite graph with directors of type FALSE and companies of type TRUE.}
\usage{data(finance)}
\format{
  The format is:
IGRAPH UN-B 11 13 -- 
+ attr: type (v/l), name (v/c)
+ edges (vertex names):
 [1] Beit, A.          --De.Beers         
 [2] Beit, A.          --Rand.Mines       
 [3] Beit, A.          --Chartered.Company
 [4] Jameson, L. S.    --De.Beers         
 [5] Jameson, L. S.    --Chartered.Company
 [6] Maguire, R.       --Goldfields       
 [7] Maguire, R.       --Chartered.Company
 [8] Michell, Sir L. L.--De.Beers         
 [9] Michell, Sir L. L.--Chartered.Company
[10] Neumann, S.       --Premier          
[11] Neumann, S.       --Rand.Mines       
[12] Wernher, -        --De.Beers         
[13] Wernher, -        --Rand.Mines       
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
data(finance)
## maybe str(finance) ; plot(finance) ...
}
\keyword{datasets}
