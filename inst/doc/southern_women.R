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
data(ddggs.clique)
get.incidence(ddggs.clique)

## ----, echo = FALSE, fig.height = 5--------------------------------------
ddggs.clique <- anPlotSpecs(ddggs.clique)
V(ddggs.clique)$label <- c(LETTERS[1:5], 1:5)
V(ddggs.clique)$label.color <- "white"
set.seed(10)
plot(ddggs.clique,
     layout = layout.fruchterman.reingold(ddggs.clique, niter = 100))

## ------------------------------------------------------------------------
ddggs.clique.proj <- actor.projection(ddggs.clique)
tc <- simple.triad.census(ddggs.clique.proj, add.names = TRUE)
tc

## ------------------------------------------------------------------------
antc <- triad.census.an(ddggs.clique, add.names = TRUE)
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
C.local <- transitivity(ddggs.clique.proj, type = 'local')
names(C.local) <- V(ddggs.clique.proj)$name
C.local

## ------------------------------------------------------------------------
exclWedges <- excl.transitivity(ddggs.clique, type = "")
exclWedges

## ------------------------------------------------------------------------
sum(exclWedges[, 2]) / sum(exclWedges[, 1])  # global
exclWedges[, 2] / exclWedges[, 1]            # local

## ------------------------------------------------------------------------
C.local.dat <- cbind(
    C = C.local,
    OpsahlC = opsahl.transitivity(ddggs.clique, type = 'local'),
    exclC = excl.transitivity(ddggs.clique, type = 'local')
)
rownames(C.local.dat) <- V(ddggs.clique.proj)$name
C.local.dat

## ----, fig.height = 4----------------------------------------------------
ddc <- data.frame(k = degree(ddggs.clique.proj),
                  C = transitivity(ddggs.clique.proj, type = 'local'))
print(ddc)
plot(aggregate(ddc$C, by = list(ddc$k), FUN = mean), pch = 19, type = 'b',
     main = 'Degree-dependent local clustering',
     xlab = 'Degree', ylab = 'Mean conditional local clustering coefficient')

## ----, fig.height = 5----------------------------------------------------
data(ddggs.group)
ddggs.group <- anPlotSpecs(ddggs.group)
V(ddggs.group)$label <- substr(V(ddggs.group)$name, 1,
                          ifelse(V(ddggs.group)$type, 5, 2))
V(ddggs.group)$label.color <- "white"
set.seed(2)
plot(ddggs.group, layout = layout.bipartite(ddggs.group))

## ----, fig.height = 4----------------------------------------------------
ddggs.group.proj <- actor.projection(ddggs.group)
ddc2 <- data.frame(
    k = degree(ddggs.group.proj),
    C = transitivity(ddggs.group.proj, type = 'local')
)
print(ddc2)
plot(aggregate(ddc2$C, by = list(k = ddc2$k), FUN = mean),
     pch = 19, type = 'b',
     main = 'Degree-dependent local clustering',
     xlab = 'Degree', ylab = 'Mean conditional local clustering coefficient')

## ----, fig.height = 4----------------------------------------------------
ddggs.group.wedges <- opsahl.transitivity(ddggs.group, type = '')
ddggs.group.wedges <- cbind(
    ddggs.group.wedges,
    ddggs.group.wedges[, 2] / ddggs.group.wedges[, 1]
)
plot(aggregate(ddggs.group.wedges[, 3],
               by = list(ddggs.group.wedges[, 1]), FUN = mean),
     pch = 19, type = 'b',
     main = 'Wedge-dependent local clustering (Opsahl)',
     xlab = 'Wedges', ylab = 'Mean conditional local clustering coefficient')

## ----, fig.height = 4----------------------------------------------------
ddggs.group.wedges <- excl.transitivity(ddggs.group, type = '')
ddggs.group.wedges <- cbind(
    ddggs.group.wedges,
    C = ddggs.group.wedges[, 2] / ddggs.group.wedges[, 1]
)
plot(aggregate(ddggs.group.wedges[, 3],
               by = list(ddggs.group.wedges[, 1]), FUN = mean),
     pch = 19, type = 'b',
     main = 'Wedge-dependent local clustering (exclusive)',
     xlab = 'Wedges', ylab = 'Mean conditional local clustering coefficient')

