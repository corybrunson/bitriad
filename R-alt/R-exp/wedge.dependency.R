#' Wedge-dependency table
#' 
#' Given a table of local wedge counts and closed wedge counts by node, return
#' a table of node counts and closed wedge proportions (mean local clustering
#' coefficients) by local wedge count.
#' @param wedges A table of two columns: wedge count and closed wedge count
#' @export

wedge.dependency <-
function(wedges) {
    agg <- aggregate(wedges[, 2], by = list(wedges[, 1]),
                     FUN = function(x) c(length(x), mean(x)))
    df <- as.data.frame(cbind(agg$Group.1, agg$x))
    names(df) <- c('l', 'n', 'C')
    df$C <- df$C / df$l
    df <- df[which(df$l > 0), ]
    return(df)
}
