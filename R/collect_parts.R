#' Collect together the different elements that make up the IPM dynamic model
#'
#' @description
#' Function \code{collect_parts} takes inputs corresponding to the different elements
#' of the IPM model
#'
#'
#' @param seedlings a \code{sf} object where the field \code{seedlings} includes
#' the projection of the seedlings' abundance.
#' @param saplings same as for the \code{seedlings} input, but for saplings
#' @param ingrowth distribution of ingrowth trees.
#' @param adults the new distribution of adults, which is the result of applying the
#' IPM methodology (only the tree survival and growth part) to the previous distribution
#' of adults.
#' @param verbose \code{logical}, if set to TRUE a progress bar will be printed.
#'
#' @return
#' A new \code{sf} object which is the projection of the input 'a'.
#'
#' @export
#'
collect_parts <- function(a, data = list(), verbose = T) {


  # country parameter.
  country <- match.arg(get_parameters(a, "country")[[1]], c("spain", "france", "usa"))
  stopifnot("Only 'country'  = 'spain' has been implemented" = country == "spain")


  # Plots must match.
  if (country == "spain") {
    seedlings <- data$seedlings
    saplings <- data$saplings
    ingrowth <- data$ingrowth
    adults <- data$adults
    stopifnot("Plot identifiers do not match" =
                (identical(a$idplot, seedlings$idplot)) &
                (identical(a$idplot, saplings$idplot)) &
                (identical(a$idplot, ingrowth$idplot)) &
                (identical(a$idplot, adults$idplot)))

    # First checks.
    stopifnot("Input 'a' must be an 'sf' object" = inherits(a, "sf"))
    stopifnot("Input 'seedlings' must be an 'sf' object" = inherits(seedlings, "sf"))
    stopifnot("Input 'saplings' must be an 'sf' object" = inherits(saplings, "sf"))
    stopifnot("Input 'ingrowth' must be an 'sf' object" = inherits(ingrowth, "sf"))
    stopifnot("Input 'adults' must be an 'sf' object" = inherits(adults, "sf"))
    stopifnot("Input 'data' list cannot be empty" = length(data) > 0)
  }


  # If verbose is TRUE, print a progress bar.
  if (verbose) {
    fname <- as.character(match.call()[[1]])
    cat(paste0("\n -> ", fname, ": Collecting seedling, sapling, ingrowth and adult tree data...\n"))
    pb <- utils::txtProgressBar(min = 0,
                                max = length(a$idplot),
                                style = 3,
                                width = 50,
                                char = "=")
  }


  # Small function to be used below.
  f <- function(z) if (is.null(z)) 0 else z

  icount = 0
  for (i in 1:nrow(a)) {

    # Progress bar.
    icount <- icount + 1
    if (verbose) setTxtProgressBar(pb, icount)

    if (country == "spain") {

      # Only applicable if stand_type == "ipm".
      if (a[i, ]$stand_type == "ipm") {


        # Retrieve young trees.
        a[i, ]$seedlings[[1]] <- if (length(seedlings[i, ]$seedlings[[1]]) > 0) seedlings[i, ]$seedlings[[1]] else list()
        a[i, ]$saplings[[1]] <- if (length(saplings[i, ]$saplings[[1]]) > 0) saplings[i, ]$saplings[[1]] else list()


        # Read ingrowth and adult data.
        x <- y <- species_ingrowth <- species_trees <- NULL
        if (length(ingrowth[i, ]$trees[[1]]) > 0) {
          x <- ingrowth[i, ]$trees[[1]]
          species_ingrowth <- names(x)
        }


        if (length(adults[i, ]$trees[[1]]) > 0) {
          y <- adults[i, ]$trees[[1]]
          species_trees <- names(y)
        }


        # Species present in the plot.
        species_unique <- unique(c(species_ingrowth, species_trees))


        # Adding adults.
        if (length(species_unique) > 0) {
          for (j in species_unique) {
            if (length(x[[j]]) > 0 | length(y[[j]]) > 0) {
              a[i, ]$trees[[1]][[j]] <- f(x[[j]]) + f(y[[j]])
            }
          }
        }
      }
    }
  }

  if (verbose) cat("\n")


  return(a)
}
