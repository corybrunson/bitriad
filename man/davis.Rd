\name{davis}
\alias{davis}
\docType{data}
\title{Davis et al southern (white) women}
\description{The data consist of 14 events attended by 18 white women in a rural town in the 1930s deep south, forming an affiliation network. They are stored as a bipartite graph with women of type FALSE and events of type TRUE.}
\usage{data(davis)}
\format{
  The format is:
IGRAPH UN-B 32 89 -- 
+ attr: name (v/c), type (v/l)
+ edges (vertex names):
Evelyn -- 1, 2, 3, 4, 5, 6, 8, 9
Laura -- 1, 2, 3, 5, 6, 8, 7
Theresa -- 2, 3, 4, 5, 6, 8, 9, 7
Brenda -- 1, 3, 4, 5, 6, 8, 7
Charlotte -- 3, 4, 5, 7
Frances -- 3, 5, 6, 8
Eleanor -- 5, 6, 8, 7
Pearl -- 6, 8, 9
Ruth -- 5, 8, 9, 7
Verne -- 8, 9, 7, 12
Myra -- 8, 9, 12, 10
Katherine -- 8, 9, 12, 10, 13, 14
Sylvia -- 8, 9, 7, 12, 10, 13, 14
Nora -- 6, 9, 7, 12, 10, 13, 14, 11
Helen -- 8, 7, 12, 10, 11
Dorothy -- 8, 9
Olivia -- 9, 11
Flora -- 9, 11
1 -- Evelyn, Laura, Brenda
2 -- Evelyn, Laura, Theresa
3 -- Evelyn, Laura, Theresa, Brenda, Charlotte, Frances
4 -- Evelyn, Theresa, Brenda, Charlotte
5 -- Evelyn, Laura, Theresa, Brenda, Charlotte, Frances, Eleanor,
             Ruth
6 -- Evelyn, Laura, Theresa, Brenda, Frances, Eleanor, Pearl, Nora
8 -- Evelyn, Laura, Theresa, Brenda, Frances, Eleanor, Pearl,
             Ruth, Verne, Myra, Katherine, Sylvia, Helen, Dorothy
9 -- Evelyn, Theresa, Pearl, Ruth, Verne, Myra, Katherine, Sylvia,
             Nora, Dorothy, Olivia, Flora
7 -- Laura, Theresa, Brenda, Charlotte, Eleanor, Ruth, Verne,
             Sylvia, Nora, Helen
12 -- Verne, Myra, Katherine, Sylvia, Nora, Helen
10 -- Myra, Katherine, Sylvia, Nora, Helen
13 -- Katherine, Sylvia, Nora
14 -- Katherine, Sylvia, Nora
11 -- Nora, Helen, Olivia, Flora
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
data(davis)
## maybe str(davis) ; plot(davis) ...
}
\keyword{datasets}
