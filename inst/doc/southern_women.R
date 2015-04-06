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

## ----, fig.height = 5----------------------------------------------------
ddc <- data.frame(k = degree(davis.clique.proj),
                  C = transitivity(davis.clique.proj, type = 'local'))
print(ddc)
plot(aggregate(ddc$C, by = list(ddc$k), FUN = mean), pch = 19, type = 'b',
     main = 'Degree-dependent local clustering',
     xlab = 'Degree', ylab = 'Mean conditional local clustering coefficient')

## ----, fig.height = 6----------------------------------------------------
data(davis.group)
davis.group <- anPlotSpecs(davis.group)
V(davis.group)$label <- substr(V(davis.group)$name, 1,
                          ifelse(V(davis.group)$type, 5, 2))
V(davis.group)$label.color <- "white"
set.seed(2)
plot(davis.group, layout = layout.bipartite(davis.group))

## ----, fig.height = 5----------------------------------------------------
davis.group.proj <- actor.projection(davis.group)
ddc2 <- data.frame(
    k = degree(davis.group.proj),
    C = transitivity(davis.group.proj, type = 'local')
)
print(ddc2)
plot(aggregate(ddc2$C, by = list(k = ddc2$k), FUN = mean),
     pch = 19, type = 'b',
     main = 'Degree-dependent local clustering',
     xlab = 'Degree', ylab = 'Mean conditional local clustering coefficient')

## ----, fig.height = 5----------------------------------------------------
davis.group.wedges <- opsahl.transitivity(davis.group, type = '')
davis.group.wedges <- cbind(
    davis.group.wedges,
    davis.group.wedges[, 2] / davis.group.wedges[, 1]
)
plot(aggregate(davis.group.wedges[, 3],
               by = list(davis.group.wedges[, 1]), FUN = mean),
     pch = 19, type = 'b',
     main = 'Wedge-dependent local clustering (Opsahl)',
     xlab = 'Wedges', ylab = 'Mean conditional local clustering coefficient')

## ----, fig.height = 5----------------------------------------------------
davis.group.wedges <- excl.transitivity(davis.group, type = '')
davis.group.wedges <- cbind(
    davis.group.wedges,
    C = davis.group.wedges[, 2] / davis.group.wedges[, 1]
)
plot(aggregate(davis.group.wedges[, 3],
               by = list(davis.group.wedges[, 1]), FUN = mean),
     pch = 19, type = 'b',
     main = 'Wedge-dependent local clustering (exclusive)',
     xlab = 'Wedges', ylab = 'Mean conditional local clustering coefficient')

