#' Hall criterion
#' 
#' Test for the existence of a system of distinct representatives for a finite 
#' set. 
#' \url{www.encyclopediaofmath.org/index.php/System_of_different_representatives}
#' 
#' @param lst A list of vectors possibly having some common elements
hallCriterion <-
  function(lst)
    all(sapply(0:(2 ^ length(lst) - 1),
               function(i) {
                 w <- which(intToBits(i) == 1)
                 length(unique(unlist(lst[w]))) >= length(w)
               }))
