#' Title
#'
#' @param a a \code{sf} object containing a number of POINT geometry types.
#' @param verbose
#'
#' @return
#' @export
#'
#' @examples
get_stats <- function(a, verbose = T) {


  # Must be an "sf" object.
  stopifnot("Input 'a' must be an 'sf' object" = inherits(a, "sf"))


  # We may (or may not) need this below.
  xabs <- get_parameters(a, "integvar")
  h <- get_parameters(a, "h")


  # If verbose is TRUE, print a progress bar.
  if (verbose) {
    fname <- as.character(match.call()[[1]])
    cat(paste0("\n -> ", fname, ": Creating new 'sf' with stats per plot...\n"))
    pb <- utils::txtProgressBar(min = 0,
                                max = length(a$idplot),
                                style = 3,
                                width = 50,
                                char = "=")
  }


  # Create a new 'sf' object to store the statistics for each plot.
  x <- a[, c("geometry", "idplot", "date", "stand_type")]
  x$ba_species <- vector("list", length(x$idplot))
  x$ba <- -1
  x$ntrees_species <- vector("list", length(x$idplot))
  x$ntrees <- -1



  # Go plot by plot.
  icount = 0
  for (i in 1:length(x$idplot)) {

    # Progress bar.
    icount <- icount + 1
    if (verbose) setTxtProgressBar(pb, icount)

    b <- a[i, ]


    # Calculates basal areas for trees. If 'stand_type' is "individual", x and h are not used anyway.
    if (b$stand_type != "") {
      if (b$stand_type == "ipm") stopifnot("Parameter 'integvars' has not been specified" = !is.null(xabs))
      x$ba_species[[i]] <- calc_ba(b, xabs, h)
      x$ntrees_species[[i]] <- calc_ntrees(b, xabs, h)
      x$ba[i] <- sum(unlist(x$ba_species[[i]]$ba))
      x$ntrees[i] <- sum(unlist(x$ntrees_species[[i]]$ntrees))
    }
  }


  # Extra carriage return.
  if (verbose) cat("\n")

  return(x)

}
