## ---- echo=FALSE, results="hide", message=FALSE, warning=FALSE-----------
library(knitr)
opts_knit$set(progress = FALSE)
opts_chunk$set(
    echo = TRUE, message = FALSE, tidy = TRUE, warning = FALSE,
    fig.path = "figure/", fig.keep = "high", fig.width = 8,
    fig.height = 6, fig.align = "center"
)

## ------------------------------------------------------------------------
library(bitriad)

## ------------------------------------------------------------------------
data(women_clique)
as_incidence_matrix(women_clique)

## ---- echo=FALSE, fig.height=5-------------------------------------------
women_clique <- prettify_an(women_clique)
V(women_clique)$label <- c(LETTERS[1:5], 1:5)
V(women_clique)$label.color <- "white"
set.seed(77)
plot(women_clique,
     layout = layout.fruchterman.reingold(women_clique, niter = 100))

## ------------------------------------------------------------------------
women_clique_proj <- actor_projection(women_clique)
(tc <- triad_census(women_clique_proj, add.names = TRUE))

## ------------------------------------------------------------------------
(antc <- triad_census(women_clique, add.names = TRUE))

## ------------------------------------------------------------------------
antc_proj <- project_census(antc, add.names = TRUE)
antc_proj$binary

## ------------------------------------------------------------------------
cbind(tc,
      antc_proj$simple,
      project_census(antc_proj$binary)$simple)

## ------------------------------------------------------------------------
(C <- unname(3 * tc[4] / (tc[3] + 3 * tc[4])))

## ------------------------------------------------------------------------
(C_vec <- c(
    C = triad_closure_from_census(antc, scheme = "full", measure = "classical"),
    OpsahlC = triad_closure_from_census(antc, scheme = "full", measure = "opsahl"),
    exclC = triad_closure_from_census(antc, scheme = "full", measure = "exclusive")
))

## ------------------------------------------------------------------------
stc <- antc_proj$binary
3 * sum(stc[4, ]) / (sum(stc[3, ]) + 3 * sum(stc[4, ]))

## ------------------------------------------------------------------------
C_local <- transitivity(women_clique_proj, type = "local")
names(C_local) <- V(women_clique_proj)$name
C_local

## ------------------------------------------------------------------------
(exclWedges <- triad_closure_exclusive(women_clique, type = "raw"))

## ------------------------------------------------------------------------
sum(exclWedges[, 2]) / sum(exclWedges[, 1])  # global
exclWedges[, 2] / exclWedges[, 1]            # local

## ------------------------------------------------------------------------
C_local_dat <- cbind(
    C = C_local,
    OpsahlC = triad_closure_opsahl(women_clique, type = "local"),
    exclC = triad_closure_exclusive(women_clique, type = "local")
)
rownames(C_local_dat) <- V(women_clique_proj)$name
C_local_dat

## ---- fig.height=5-------------------------------------------------------
ddc <- data.frame(k = degree(women_clique_proj),
                  C = transitivity(women_clique_proj, type = "local"))
print(ddc)
plot(aggregate(ddc$C, by = list(ddc$k), FUN = mean), pch = 19, type = "b",
     main = "Degree-dependent local clustering",
     xlab = "Degree", ylab = "Mean conditional local clustering coefficient")

## ---- fig.height=6-------------------------------------------------------
data(women_group)
women_group <- prettify_an(women_group)
V(women_group)$label <- substr(V(women_group)$name, 1,
                               ifelse(V(women_group)$type, 5, 2))
V(women_group)$label.color <- "white"
set.seed(2)
plot(women_group, layout = layout_as_bipartite(women_group))

## ---- fig.height=5-------------------------------------------------------
women_group_proj <- actor_projection(women_group)
(ddc2 <- data.frame(
    k = degree(women_group_proj),
    C = transitivity(women_group_proj, type = "local")
))
plot(aggregate(ddc2$C, by = list(k = ddc2$k), FUN = mean),
     pch = 19, type = "b",
     main = "Degree-dependent local clustering",
     xlab = "Degree", ylab = "Mean conditional local clustering coefficient")

## ---- fig.height=5-------------------------------------------------------
women_group_wedges <- triad_closure_opsahl(women_group, type = "raw")
women_group_wedges <- cbind(
    women_group_wedges,
    women_group_wedges[, 2] / women_group_wedges[, 1]
)
plot(aggregate(women_group_wedges[, 3],
               by = list(women_group_wedges[, 1]), FUN = mean),
     pch = 19, type = "b",
     main = "Wedge-dependent local clustering (Opsahl)",
     xlab = "Wedges", ylab = "Mean conditional local clustering coefficient")

## ---- fig.height=5-------------------------------------------------------
women_group_wedges <- triad_closure_exclusive(women_group, type = "raw")
women_group_wedges <- cbind(
    women_group_wedges,
    C = women_group_wedges[, 2] / women_group_wedges[, 1]
)
plot(aggregate(women_group_wedges[, 3],
               by = list(women_group_wedges[, 1]), FUN = mean),
     pch = 19, type = "b",
     main = "Wedge-dependent local clustering (exclusive)",
     xlab = "Wedges", ylab = "Mean conditional local clustering coefficient")

