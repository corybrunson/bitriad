#' Hall criterion
#' 
#' A criterion for the existence of a system of distinct representatives for a
#' finite set:
#' www.encyclopediaofmath.org/index.php/System_of_different_representatives
#' @param lst A list of vectors possibly having some common elements

hall.criterion <-
function(lst) all(sapply(0:(2 ^ length(lst) - 1),
                                           function(i) {
    w <- which(intToBits(i) == 1)
    length(unique(unlist(lst[w]))) >= length(w)
}))
