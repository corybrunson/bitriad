\name{directors}
\alias{directors}
\docType{data}
\title{Barnes and Burkett subset of corporate directors}
\description{The data consist of a subset of overlapping corporate, university, and social club board memberships drawn from Moody's manuals for 1962, forming an affiliation network. They are stored as a bipartite graph with board members of type FALSE and affiliations of type TRUE.}
\usage{data(directors)}
\format{
  The format is:
IGRAPH UN-B 44 99 -- 
+ attr: type (v/l), name (v/c)
+ edges (vertex names):
Barr -- Commonwealth.Edison, Swift, Northwestern.Univ., Chicago,
                                            Commercial
Block -- Commonwealth.Edison, First.National.Bank.of.Chicago,
                                            Inland.Steel,
                                            Museum.of.Science.and.Industry,
                                            Commercial
Chewning -- Amour, John.Hancock.Mutual
Clark -- Amour, John.Hancock.Mutual, Chicago
Cushman -- First.National.Bank.of.Chicago, Sears.and.Roebuck
Eberhard -- Caterpillar.Tractor, First.National.Bank.of.Chicago,
                                            Chicago
Freeman -- Borg.Warner, Caterpillar.Tractor,
                                            Container.Corp..of.America,
                                            First.National.Bank.of.Chicago,
                                            Chicago, Commercial
Gale -- Commonwealth.Edison,
                                            Continental.International.Bank.and.Trust,
                                            Inland.Steel,
                                            Museum.of.Science.and.Industry,
                                            Northwestern.Univ.,
                                            Chicago, Indian.Hill
Goodrich -- International.Harvester, Swift, Chicago, Commercial
Ingersoll -- Borg.Warner, Container.Corp..of.America,
                                            First.National.Bank.of.Chicago,
                                            Chicago, Commercial,
                                            Indian.Hill
Jarvis -- Continental.International.Bank.and.Trust,
                                            International.Harvester,
                                            Swift,
                                            Museum.of.Science.and.Industry,
                                            Univ..of.Chicago,
                                            Commercial
Kennedy -- Commonwealth.Edison,
                                            Continental.International.Bank.and.Trust,
                                            International.Harvester,
                                            Swift,
                                            Univ..of.Chicago,
                                            Chicago, Commercial
Livingston -- Amour, First.National.Bank.of.Chicago, Inland.Steel,
                                            Sears.and.Roebuck,
                                            Standard.Oil,
                                            Art.Institute.of.Chicago,
                                            Univ..of.Chicago,
                                            Chicago, Commercial
McCormick -- First.National.Bank.of.Chicago,
                                            International.Harvester,
                                            Swift,
                                            Art.Institute.of.Chicago,
                                            Chicago, Commercial
McDowell -- First.National.Bank.of.Chicago, Swift, Commercial
Oates -- Chase.Manhattan.Bank, Equitable.Life.Assurance,
                                            First.National.Bank.of.Chicago,
                                            Museum.of.Science.and.Industry,
                                            Northwestern.Univ.,
                                            Century, Chicago,
                                            Commercial, Links
Prince -- Amour, First.National.Bank.of.Chicago,
                                            John.Hancock.Mutual
Rockefeller -- Chase.Manhattan.Bank, Equitable.Life.Assurance,
                                            Univ..of.Chicago,
                                            Century, Links
Swearingen -- Chase.Manhattan.Bank,
                                            First.National.Bank.of.Chicago,
                                            Standard.Oil
Ward -- Commonwealth.Edison, International.Harvester,
                                            Univ..of.Chicago,
                                            Chicago, Commercial
Amour -- Chewning, Clark, Livingston, Prince
Borg.Warner -- Freeman, Ingersoll
Caterpillar.Tractor -- Eberhard, Freeman
Chase.Manhattan.Bank -- Oates, Rockefeller, Swearingen
Commonwealth.Edison -- Barr, Block, Gale, Kennedy, Ward
Container.Corp..of.America -- Freeman, Ingersoll
Continental.International.Bank.and.Trust -- Gale, Jarvis, Kennedy
Equitable.Life.Assurance -- Oates, Rockefeller
First.National.Bank.of.Chicago -- Block, Cushman, Eberhard,
                                            Freeman, Ingersoll,
                                            Livingston, McCormick,
                                            McDowell, Oates,
                                            Prince, Swearingen
Inland.Steel -- Block, Gale, Livingston
International.Harvester -- Goodrich, Jarvis, Kennedy, McCormick,
                                            Ward
John.Hancock.Mutual -- Chewning, Clark, Prince
Sears.and.Roebuck -- Cushman, Livingston
Standard.Oil -- Livingston, Swearingen
Swift -- Barr, Goodrich, Jarvis, Kennedy, McCormick, McDowell
Art.Institute.of.Chicago -- Livingston, McCormick
Museum.of.Science.and.Industry -- Block, Gale, Jarvis, Oates
Northwestern.Univ.  -- Barr, Gale, Oates
Univ..of.Chicago -- Jarvis, Kennedy, Livingston, Rockefeller, Ward
Century -- Oates, Rockefeller
Chicago -- Barr, Clark, Eberhard, Freeman, Gale, Goodrich,
                                            Ingersoll, Kennedy,
                                            Livingston, McCormick,
                                            Oates, Ward
Commercial -- Barr, Block, Freeman, Goodrich, Ingersoll, Jarvis,
                                            Kennedy, Livingston,
                                            McCormick, McDowell,
                                            Oates, Ward
Indian.Hill -- Gale, Ingersoll
Links -- Oates, Rockefeller
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
data(directors)
## maybe str(directors) ; plot(directors) ...
}
\keyword{datasets}
