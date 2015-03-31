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

