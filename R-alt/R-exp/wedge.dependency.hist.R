#' Wedge-dependency histograms
#'
#' Draw cool strata of histograms of wedge closure rate by wedge count.
#' @param wedges A wedge list of two columns: wedge count and closed wedge count
#' @param max.no An upper bound on the wedge count to construct histograms for
#' @param wedge.nos Specific wedge counts to include (overrides max.no)

wedge.dependency.hist <-
function(wedges, max.no = 12, wedge.nos = NULL) {
    require(ggplot2)
    require(plyr)
    if(is.null(wedge.nos)) {
        V.ran <- sort(setdiff(unique(wedges[, 1]), 0))
        V.max <- if(length(V.ran) <= max.no) max(V.ran) else V.ran[max.no]
        wedge.nos <- 1:V.max
        wh <- which(wedges[, 1] != 0 & wedges[, 1] <= V.max)
    } else wh <- which(wedges[, 1] %in% wedge.nos)
    vtc <- data.frame(V = wedges[wh, 1], C = wedges[wh, 2] / wedges[wh, 1])
    w.breaks <- lapply(wedge.nos, function(v) {
        wid = 1 / v
        seq(0 - wid / 2, 1 + wid / 2, wid)
    })
    hls <- mapply(function(x, b) geom_histogram(data = x, breaks = b),
                  dlply(vtc, .(V)), w.breaks)
    return(ggplot(vtc, aes(x = C)) + hls + facet_grid(V ~ ., scales = "free_x"))
}
