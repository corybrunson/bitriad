#' Matrix dimension synchronization
#' 
#' Given a list of matrices, extend them all by the necessary zero rows to have
#' dimension the greatest number of rows by the greatest number of columns among
#' them.
#' @param lst A list of matrices

sync.mat <-
    function(lst) {
        sync.dim <- apply(sapply(lst, dim), 1, max)
        return(lapply(lst, function(mat) {
            cbind(rbind(mat,
                        matrix(0, nr = sync.dim[1] - dim(mat)[1],
                               nc = dim(mat)[2])),
                  matrix(0, nr = sync.dim[1], nc = sync.dim[2] - dim(mat)[2]))
        }))
    }
