## ----, echo = FALSE, results = 'hide', message = FALSE, warning = FALSE----
pkgs <- c('knitr')
for(pkg in pkgs) library(pkg, character.only = TRUE)
opts_knit$set(progress = FALSE)
opts_chunk$set(
    echo = TRUE, message = FALSE, tidy = TRUE, warning = FALSE,
    fig.path = "figure/", fig.keep = "high", fig.width = 8,
    fig.height = 6, fig.align = "center"
)

## ------------------------------------------------------------------------
library(bitriad)
data(davis.clique)
get.incidence(davis.clique)

## ----, echo = FALSE, fig.height = 6--------------------------------------
davis.clique <- anPlotSpecs(davis.clique)
V(davis.clique)$label <- c(LETTERS[1:5], 1:5)
V(davis.clique)$label.color <- "white"
set.seed(10)
plot(davis.clique,
     layout = layout.fruchterman.reingold(davis.clique, niter = 100))

## ------------------------------------------------------------------------
davis.clique.proj <- actor.projection(davis.clique)
tc <- simple.triad.census(davis.clique.proj, add.names = TRUE)
tc

## ------------------------------------------------------------------------
antc <- triad.census.an(davis.clique, add.names = TRUE)
antc

## ------------------------------------------------------------------------
antc.proj <- project.census(antc, add.names = TRUE)
antc.proj$structural

## ------------------------------------------------------------------------
cbind(tc,
      antc.proj$simple,
      project.census(antc.proj$structural)$simple)

## ------------------------------------------------------------------------
C <- unname(3 * tc[4] / (tc[3] + 3 * tc[4]))
C

## ------------------------------------------------------------------------
C.vec <- c(
    C = transitivity.census(antc, scheme = "full", flavor = "classical"),
    OpsahlC = transitivity.census(antc, scheme = "full", flavor = "opsahl"),
    exclC = transitivity.census(antc, scheme = "full", flavor = "exclusive")
)
C.vec

## ------------------------------------------------------------------------
stc <- antc.proj$structural
3 * sum(stc[4, ]) / (sum(stc[3, ]) + 3 * sum(stc[4, ]))

## ------------------------------------------------------------------------
C.local <- transitivity(davis.clique.proj, type = 'local')
names(C.local) <- V(davis.clique.proj)$name
C.local

## ------------------------------------------------------------------------
exclWedges <- excl.transitivity(davis.clique, type = "")
exclWedges

## ------------------------------------------------------------------------
sum(exclWedges[, 2]) / sum(exclWedges[, 1])  # global
exclWedges[, 2] / exclWedges[, 1]            # local

## ------------------------------------------------------------------------
C.local.dat <- cbind(
    C = C.local,
    OpsahlC = opsahl.transitivity(davis.clique, type = 'local'),
    exclC = excl.transitivity(davis.clique, type = 'local')
)
rownames(C.local.dat) <- V(davis.clique.proj)$name
C.local.dat

