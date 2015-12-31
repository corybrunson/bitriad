## ---- echo = FALSE, results = 'hide', message = FALSE, warning = FALSE----
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
data(women.clique)
get.incidence(women.clique)

## ---- echo = FALSE, fig.height = 6---------------------------------------
women.clique <- prettify.an(women.clique)
V(women.clique)$label <- c(LETTERS[1:5], 1:5)
V(women.clique)$label.color <- "white"
set.seed(13)
plot(women.clique,
     layout = layout.fruchterman.reingold(women.clique, niter = 100))

## ------------------------------------------------------------------------
women.clique.proj <- actor.projection(women.clique)
tc <- simple.triad.census(women.clique.proj, add.names = TRUE)
tc

## ------------------------------------------------------------------------
antc <- triad.census.an(women.clique, add.names = TRUE)
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
C.local <- transitivity(women.clique.proj, type = 'local')
names(C.local) <- V(women.clique.proj)$name
C.local

## ------------------------------------------------------------------------
exclWedges <- excl.transitivity(women.clique, type = "")
exclWedges

## ------------------------------------------------------------------------
sum(exclWedges[, 2]) / sum(exclWedges[, 1])  # global
exclWedges[, 2] / exclWedges[, 1]            # local

## ------------------------------------------------------------------------
C.local.dat <- cbind(
    C = C.local,
    OpsahlC = opsahl.transitivity(women.clique, type = 'local'),
    exclC = excl.transitivity(women.clique, type = 'local')
)
rownames(C.local.dat) <- V(women.clique.proj)$name
C.local.dat

## ---- fig.height = 5-----------------------------------------------------
ddc <- data.frame(k = degree(women.clique.proj),
                  C = transitivity(women.clique.proj, type = 'local'))
print(ddc)
plot(aggregate(ddc$C, by = list(ddc$k), FUN = mean), pch = 19, type = 'b',
     main = 'Degree-dependent local clustering',
     xlab = 'Degree', ylab = 'Mean conditional local clustering coefficient')

## ---- fig.height = 6-----------------------------------------------------
data(women.group)
women.group <- prettify.an(women.group)
V(women.group)$label <- substr(V(women.group)$name, 1,
                          ifelse(V(women.group)$type, 5, 2))
V(women.group)$label.color <- "white"
set.seed(2)
plot(women.group, layout = layout.bipartite(women.group))

## ---- fig.height = 5-----------------------------------------------------
women.group.proj <- actor.projection(women.group)
ddc2 <- data.frame(
    k = degree(women.group.proj),
    C = transitivity(women.group.proj, type = 'local')
)
print(ddc2)
plot(aggregate(ddc2$C, by = list(k = ddc2$k), FUN = mean),
     pch = 19, type = 'b',
     main = 'Degree-dependent local clustering',
     xlab = 'Degree', ylab = 'Mean conditional local clustering coefficient')

## ---- fig.height = 5-----------------------------------------------------
women.group.wedges <- opsahl.transitivity(women.group, type = '')
women.group.wedges <- cbind(
    women.group.wedges,
    women.group.wedges[, 2] / women.group.wedges[, 1]
)
plot(aggregate(women.group.wedges[, 3],
               by = list(women.group.wedges[, 1]), FUN = mean),
     pch = 19, type = 'b',
     main = 'Wedge-dependent local clustering (Opsahl)',
     xlab = 'Wedges', ylab = 'Mean conditional local clustering coefficient')

## ---- fig.height = 5-----------------------------------------------------
women.group.wedges <- excl.transitivity(women.group, type = '')
women.group.wedges <- cbind(
    women.group.wedges,
    C = women.group.wedges[, 2] / women.group.wedges[, 1]
)
plot(aggregate(women.group.wedges[, 3],
               by = list(women.group.wedges[, 1]), FUN = mean),
     pch = 19, type = 'b',
     main = 'Wedge-dependent local clustering (exclusive)',
     xlab = 'Wedges', ylab = 'Mean conditional local clustering coefficient')

