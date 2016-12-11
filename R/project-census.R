#' Project a triad census
#' 
#' This function inputes an affiliation network triad census of any scheme and
#' returns a list of triad censuses projected from it (not icluding itself). In
#' order of projectability, the census schemes are full (affiliation network),
#' uniformity, structural, and simple.
#' 
#' @name project_census
#' @param census Numeric matrix or vector; the input triad census. It is
#' classified as "uniformity", "structural", or "simple" if its dimensons are
#' 8-by-2, 4-by-2, or 4-by-1, respectively, or unless otherwise specified;
#' otherwise it is classified as "full".
#' @param scheme Character; the type of census (to be matched to "full",
#' "uniformity", "structural", or "simple").
#' @param add.names Logical; whether to label the rows and (where applicable)
#' columns of the projected censuses.
#' @export
project_census <-
  function(census, scheme = NULL, add.names = FALSE) {
    # Put into matrix form (single column if vector)
    census <- as.matrix(census)
    # Decide what kind of census it is
    cdim <- dim(census)
    if(!is.null(scheme)) {
      scheme <- match.arg(scheme,
                          c("full", "uniformity", "structural", "simple"))
      if((scheme == "uniformity" & !all(cdim == c(8, 2))) |
         (scheme == "structural" & !all(cdim == c(4, 2))) |
         (scheme == "simple" & !all(cdim == c(4, 1)))) {
        scheme <- NULL
        warning('Incongruent input; coercing to scheme "full".')
      }
    }
    if(!is.null(scheme)) {
      if(!(scheme %in% c("full", "uniformity", "structural", "simple")) |
         (scheme == "uniformity" & !all(cdim == c(8, 2))) |
         (scheme == "structural" & !all(cdim == c(4, 2))) |
         (scheme == "simple" & !all(cdim == c(4, 1)))) {
        scheme <- NULL
        warning('Incongruent input; coercing to scheme "full".')
      }
    }
    if(is.null(scheme)) {
      scheme <- if(all(cdim == c(8, 2))) "uniformity" else
        if(all(cdim == c(4, 2))) "structural" else
          if(all(cdim == c(4, 1))) "simple" else
            "full"
    }
    # Initiate census list
    censuses <- list()
    # If "full", project to "uniformity"
    if(scheme == "full") {
      census <- ftc2utc(census)
      if(add.names) {
        rownames(census) <- 0:7 # FIX THIS
        colnames(census) <- 0:1
      }
      scheme <- "uniformity"
      censuses <- c(list(uniformity = census), censuses)
    }
    # If "uniformity", project to "structural"
    if(scheme == "uniformity") {
      census <- utc2stc(census)
      if(add.names) {
        rownames(census) <- 0:3
        colnames(census) <- 0:1
      }
      scheme <- "structural"
      censuses <- c(list(structural = census), censuses)
    }
    # If "structural", project to "simple"
    if(scheme == "structural") {
      census <- stc2tc(census)
      if(add.names) {
        names(census) <- 0:3
      }
      scheme <- "simple"
      censuses <- c(list(simple = census), censuses)
    }
    # If "simple", project to "total"
    if(scheme == "simple") {
      censuses <- c(list(total = sum(census)), censuses)
    }
    # Return list
    censuses
  }

#' @rdname project_census
ftc2utc <-
  function(census) {
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
ftc2stc <-
  function(census) {
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
ftc2tc <-
  function(census) {
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
utc2stc <-
  function(census) {
    return(matrix(c(
      census[1, 1],
      census[2, 1],
      sum(census[3:4, 1]),
      sum(census[5:8, 1]),
      census[1, 2],
      census[2, 2],
      sum(census[3:4, 2]),
      sum(census[5:8, 2])
    ), nrow = 4, ncol = 2))
  }

#' @rdname project_census
utc2tc <- function(census) stc2tc(utc2stc(census))

#' @rdname project_census
stc2tc <-
  function(census) {
    c(
      census[1:3, 1],
      sum(c(census[4, 1], census[1:4, 2]))
    )
  }
