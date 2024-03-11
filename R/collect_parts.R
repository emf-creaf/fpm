#' Title
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
#' @export
#'
#' @examples
collect_parts <- function(seedlings, saplings, ingrowth, adults, verbose = T) {

  # First checks.
  stopifnot("Input 'seedlings' must be an 'sf' object" = inherits(seedlings, "sf"))
  stopifnot("Input 'saplings' must be an 'sf' object" = inherits(saplings, "sf"))
  stopifnot("Input 'ingrowth' must be an 'sf' object" = inherits(ingrowth, "sf"))
  stopifnot("Input 'adults' must be an 'sf' object" = inherits(adults, "sf"))


  # Plots must match.
  stopifnot("Plot identifiers do not match" =
              (identical(seedlings$idplot, adults$idplot) &
                 identical(saplings$idplot, adults$idplot) &
                 identical(ingrowth$idplot, adults$idplot)))


  # Countries must match.
  countries <- c("spain", "france", "usa")
  country <- c(match.arg(get_parameters(seedlings, "country")[[1]], countries),
               match.arg(get_parameters(saplings, "country")[[1]], countries),
               match.arg(get_parameters(ingrowth, "country")[[1]], countries),
               match.arg(get_parameters(adults, "country")[[1]], countries))
  country <- unique(country)
  stopifnot("Parameter 'country' in inputs do not match" = length(unique(country)) == 1)
  stopifnot("Only 'country'  = 'spain' has been implemented" = country == "spain")


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


  # New stand 'sf' object.
  b <- clear_stands(seedlings)


  # Small function to be used below.
  f <- function(z) if (is.null(z)) 0 else z

  icount = 0
  for (i in 1:nrow(b)) {

    # Progress bar.
    icount <- icount + 1
    if (verbose) setTxtProgressBar(pb, icount)


    if (country == "spain") {

      # Retrieve young trees.
      if (!is.null(seedlings$seedlings[[i]])) b$seedlings[[i]] <- seedlings$seedlings[[i]]
      if (!is.null(saplings$saplings[[i]]))b$saplings[[i]] <- saplings$saplings[[i]]


      # Read ingrowth data.
      x <- ingrowth[i, ]$trees[[1]]
      species_ingrowth <- names(x)


      # Read adult trees data
      y <- adults[i, ]$trees[[1]]
      species_trees <- names(y)


      # Species present in the plot.
      species_unique <- unique(c(species_ingrowth, species_trees))


      # Adding adults.
      if (length(species_unique) > 0) {
        for (j in species_unique) b[i, ]$trees[[1]][[j]] <- f(x[[j]]) + f(y[[j]])
      }
    }
  }

  if (verbose) cat("\n")


  return(b)
}
