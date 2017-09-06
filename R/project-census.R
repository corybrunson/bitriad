#' @title Project a higher-resolution triad census to a lower-resolution one
#'   
#' @description Given a triad census of any scheme, construct a triad census of 
#'   a coarser (strictly less informative) scheme.
#'   
#' @details This function inputes an affiliation network triad census of any 
#'   scheme and returns a list of triad censuses projected from it (not icluding
#'   itself). The schemes are, in order of resolution, \emph{full} (also called 
#'   the \emph{affiliation network triad census} without qualification), 
#'   \emph{difference}, \emph{binary}, and \emph{simple}. A final element of the
#'   output list is the total number of triads in the affiliation network. Each
#'   summary can be recovered from those before it, specifically by aggregating
#'   certain matrix entries to form a smaller matrix. The helper functions
#'   \code{*_from_*_census()} project a census of each scheme to one of each
#'   coarser scheme.
#'   
#' @template triadcensus
#'   
#' @name project_census
#' @family triad census functions
#' @param census Numeric matrix or vector; an affiliation network triad census. 
#'   It is treated as binary or simple if its dimensons are 4-by-2 or 4-by-1, 
#'   respectively, unless otherwise specified by \code{scheme}; otherwise it is 
#'   treated as full.
#' @param scheme Character; the type of triad census provided, matched to 
#'   \code{"full"}, \code{"difference"} (also \code{"uniformity"}), 
#'   \code{"binary"} (also \code{"structural"}), or \code{"simple"}.
#' @param add.names Logical; whether to label the rows and columns of the output
#'   matrix.
#' @export
project_census <- function(
  census, scheme = NULL,
  add.names = FALSE
) {
  # put into matrix form (single column if vector)
  census <- as.matrix(census)
  # identify the census scheme
  scheme <- census_scheme(census = census, scheme = scheme)
  
  # initiate census list
  censuses <- list()
  # if "full", project to "difference"
  if (scheme == "full") {
    census <- difference_from_full_census(census)
    if (add.names) {
      dimnames(census) <- list(
        paste0(
          "(",
          apply(expand.grid(0:1, 0:1, 0:1), 1, paste, collapse = ","),
          ")"
        ),
        0:1
      )
    }
    scheme <- "difference"
    censuses <- c(list(difference = census), censuses)
  }
  # if "difference", project to "binary"
  if (scheme == "difference") {
    census <- binary_from_difference_census(census)
    if (add.names) {
      dimnames(census) <- list(0:3, 0:1)
    }
    scheme <- "binary"
    censuses <- c(list(binary = census), censuses)
  }
  # if "binary", project to "simple"
  if (scheme == "binary") {
    census <- simple_from_binary_census(census)
    if (add.names) {
      names(census) <- 0:3
    }
    scheme <- "simple"
    censuses <- c(list(simple = census), censuses)
  }
  # if "simple", project to "total"
  if(scheme == "simple") {
    censuses <- c(list(total = sum(census)), censuses)
  }
  
  # return list
  censuses
}

#' @rdname project_census
#' @export
project.census <- project_census

#' @rdname project_census
#' @export
difference_from_full_census <- function(census) {
  # Trivial cases
  if(sum(census) == 0) return(matrix(0, nrow = 8, ncol = 2))
  if(all(dim(census) == 1)) return(matrix(c(census[1, 1], rep(0, 15)),
                                          nrow = 8, ncol = 2))
  # Which increments in pw excl event counts (nondecreasing) are positive
  i2l <- sapply(0:(nrow(census) - 1), function(i) {
    lambda <- index_partition(i)
    ed <- c(0, lambda[3:2]) < lambda[3:1]
    return(sum((2 ^ (2:0)) * ed) + 1)
  })
  wh <- lapply(1:8, function(i) which(i2l == i))
  matrix(c(
    # Empty triads
    census[1, 1],
    # No triad event; 1, 2, and 3 pairs connected
    sapply(2:8, function(i) sum(census[wh[[i]], 1])),
    # Committees with no pairwise events
    sum(census[1, -1]),
    # Triad event; 1, 2, and 3 pairs connected
    sapply(2:8, function(i) sum(census[wh[[i]], -1]))
  ), nrow = 8, ncol = 2)
}

#' @rdname project_census
ftc2utc <- function(census) {
  .Deprecated("difference_from_full_census")
  difference_from_full_census(census)
}

#' @rdname project_census
#' @export
binary_from_full_census <- function(census) {
  # Trivial cases
  if(sum(census) == 0) return(matrix(0, nrow = 4, ncol = 2))
  if(all(dim(census) == 1)) return(matrix(c(census[1, 1], rep(0, 7)),
                                          nrow = 4, ncol = 2))
  # No. edges (1, 2, or 3) induced by each nonempty lambda with w = 0
  pw_counts <- sapply(1:(dim(census)[1] - 1), function(i) {
    length(which(index_partition(i) > 0))
  })
  # Which rows connect 1, 2, and 3 pairs
  wh <- lapply(1:3, function(i) which(pw_counts == i) + 1)
  matrix(c(
    # Empty triads
    census[1, 1],
    # No triad event; 1, 2, and 3 pairs connected
    sum(census[wh[[1]], 1]),
    sum(census[wh[[2]], 1]),
    sum(census[wh[[3]], 1]),
    # Committees with no pairwise events
    sum(census[1, 2:dim(census)[2]]),
    # Triad event; 1, 2, and 3 pairs connected
    sum(census[wh[[1]], 2:dim(census)[2]]),
    sum(census[wh[[2]], 2:dim(census)[2]]),
    sum(census[wh[[3]], 2:dim(census)[2]])
  ), nrow = 4, ncol = 2)
}

#' @rdname project_census
ftc2stc <- function(census) {
  .Deprecated("binary_from_full_census")
  binary_from_full_census(census = census)
}

#' @rdname project_census
#' @export
simple_from_full_census <- function(census) {
  # Trivial cases
  if(sum(census) == 0) return(rep(0, 4))
  if(all(dim(census) == 1)) return(c(census[1, 1], 0, 0, 0))
  # Number of edges (1, 2, or 3) induced by each lambda except c(0, 0, 0)
  pw_counts <- sapply(1:(dim(census)[1] - 1), function(i) {
    length(which(index_partition(i) > 0))
  })
  c(
    # Empty triads all have lambda = c(0, 0, 0), w = 0
    census[1, 1],
    # Dyads
    sum(census[which(pw_counts == 1) + 1, 1]),
    # Intransitive wedges
    sum(census[which(pw_counts == 2) + 1, 1]),
    # Triangles, including all columns w > 0
    sum(census[which(pw_counts == 3) + 1, 1]) +
      ifelse(dim(census)[2] == 1, 0, sum(census[, 2:dim(census)[2]]))
  )
}

#' @rdname project_census
ftc2tc <- function(census) {
  .Deprecated("simple_from_full_census")
  simple_from_full_census(census)
}

#' @rdname project_census
#' @export
binary_from_difference_census <- function(census) {
  matrix(c(
    census[1, 1],
    census[2, 1],
    sum(census[3:4, 1]),
    sum(census[5:8, 1]),
    census[1, 2],
    census[2, 2],
    sum(census[3:4, 2]),
    sum(census[5:8, 2])
  ), nrow = 4, ncol = 2)
}

#' @rdname project_census
utc2stc <- function(census) {
  .Deprecated("binary_from_difference_census")
  binary_from_difference_census(census)
}

#' @rdname project_census
#' @export
simple_from_difference_census <- function(census)
  simple_from_binary_census(binary_from_difference_census(census))

#' @rdname project_census
utc2tc <- function(census) {
  .Deprecated("simple_from_difference_census")
  simple_from_difference_census(census)
}

#' @rdname project_census
#' @export
simple_from_binary_census <- function(census) {
  c(
    census[1:3, 1],
    sum(c(census[4, 1], census[1:4, 2]))
  )
}

#' @rdname project_census
stc2tc <- function(census) {
  .Deprecated("simple_from_binary_census")
  simple_from_binary_census(census)
}
