#' @title Project a higher-resolution triad census to a lower-resolution one
#'   
#' @description Given a triad census of any scheme, construct a triad census of 
#'   a coarser (strictly less informative) scheme.
#'   
#' @details This function inputes an affiliation network triad census of any 
#'   scheme and returns a list of triad censuses projected from it (not icluding
#'   itself). The schemes are, in order of resolution, \emph{full} (also called 
#'   the \emph{affiliation network triad census} without qualification), 
#'   \emph{binary} (or \emph{structural}), and \emph{simple}. A final element of
#'   the output list is the total number of triads in the affiliation network. 
#'   Each summary can be recovered from those before it, specifically by 
#'   aggregating certain matrix entries to form a smaller matrix. The helper
#'   functions \code{*_from_*_census()} project a census of each scheme to one
#'   of each coarser scheme.
#'   
#' @template triadcensus
#'   
#' @name project_census
#' @param census Numeric matrix or vector; an affiliation network triad census. 
#'   It is treated as binary or simple if its dimensons are 4-by-2 or 4-by-1, 
#'   respectively, unless otherwise specified by \code{scheme}; otherwise it is 
#'   treated as full.
#' @param scheme Character; the type of triad census to calculate, matched to 
#'   \code{"full"}, \code{"binary"} (equivalently, \code{"structural"}), or 
#'   \code{"simple"}.
#' @param add.names Logical; whether to label the rows and columns of the output
#'   matrix.
#' @export
project_census <- function(
  census,
  scheme = NULL,
  add.names = FALSE
) {
  # put into matrix form (single column if vector)
  census <- as.matrix(census)
  # identify the census scheme
  cdim <- dim(census)
  if (!is.null(scheme)) {
    scheme <- match.arg(scheme, c("full", "binary", "structural", "simple"))
    if (scheme %in% c("binary", "structural") & any(cdim != c(4, 2))) {
      warning("A binary census must be formatted as a 4-by-2 matrix; ",
              "the input census will be treated as a full census.")
    }
    if (scheme == "simple" & any(cdim != c(4, 1))) {
      warning("A simple census must be formatted ",
              "as a 4-by-1 matrix or as a length-4 vector; ",
              "the input census will be treated as a full census.")
    }
  } else {
    scheme <- if (all(cdim == c(4, 2))) {
      "binary"
    } else  if (all(cdim == c(4, 1))) {
      "simple"
    } else {
      "full"
    }
  }
  
  # initiate census list
  censuses <- list()
  # if "full", project to "binary"
  if (scheme == "full") {
    census <- ftc2stc(census)
    if (add.names) {
      dimnames(census) <- list(0:3, 0:1)
    }
    scheme <- "binary"
    censuses <- c(list(binary = census), censuses)
  }
  # if "binary", project to "simple"
  if (scheme == "binary") {
    census <- stc2tc(census)
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
ftc2stc <- binary_from_full_census

#' @rdname project_census
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
      ifelse(dim(census)[2] == 1, 0, sum(census[, 2:dim(census)[2]])))
}

#' @rdname project_census
ftc2tc <- simple_from_full_census

#' @rdname project_census
simple_from_binary_census <- function(census) {
  c(
    census[1:3, 1],
    sum(c(census[4, 1], census[1:4, 2]))
  )
}

#' @rdname project_census
stc2tc <- simple_from_binary_census
