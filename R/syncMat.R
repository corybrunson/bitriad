#' Matrix dimension synchronization
#' 
#' This function extends each member of a list of matrices, extend them all by
#' the necessary zero rows and columns to have dimension the greatest number of
#' rows by the greatest number of columns among them.
#' 
#' @param lst A list of matrices.
syncMat <-
  function(lst) {
    sync_dim <- apply(sapply(lst, dim), 1, max)
    return(lapply(lst, function(mat) {
      cbind(rbind(mat,
                  matrix(0, nrow = sync_dim[1] - dim(mat)[1],
                         ncol = dim(mat)[2])),
            matrix(0, nrow = sync_dim[1], ncol = sync_dim[2] - dim(mat)[2]))
    }))
  }
