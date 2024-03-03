#' Extract species names of \code{sf} stand object.
#'
#' @description
#' Extract names of all species (i.e. seedlings, saplings, trees) per plot and stores their
#' names in a new field.
#'
#' @param a \code{sf} object containing a number of POINT geometry types
#' @param verbose logical, if set to TRUE a progress bar will be printed on screen.
#'
#' @details
#' Additional details...
#'
#' @return
#' Same \code{sf} object with three additional fields. The first one, named "species",
#' includes a list with three elements, "trees", "seedlings" and "saplings". Each one of these
#' elements contains the corresponding species names per plot. The second field, names "species_all",
#' will be a character vector containing the names of all species in that particular plot, irrespective
#' of whether they correspond to trees, seedlings or saplings. The third field, names "nspecies", will
#' be a single number with the total number of species of trees, seedlings or saplings, present in that
#' plot. Therefore, this last field is simply the result of applying function \code{length} to "nspecies".
#'
#' @export
#'
#' @examples
get_species<- function(a, verbose = T) {

  # Must be an "sf" object.
  stopifnot("Input 'a' must be an 'sf' object" = inherits(a, "sf"))


  # If progress is TRUE, print a progress bar.
  if (verbose) {

    fname <- as.character(match.call()[[1]])
    cat(paste0("\n -> ", fname, ": Creating new fields in input 'sf' with all species names per plot...\n"))
    pb <- txtProgressBar(min = 0,
                         max = length(nrow(a)),
                         style = 3,
                         width = 50,
                         char = "=")
  }


  # Retrieve parameters.
  p <- a |> get_parameters("country")
  country <- p$country
  data_type <- data$data_type
  stand_type <- data$stand_type


  # Create a new 'sf' object to store the names of the species per plot.
  x <- a
  if (country == "spain") {
    x$trees <- x$seedlings <- x$saplings <- NULL
    x$species <- x$species_all <- vector("list", length(x$idplot))
    x$nspecies <- 0
  } else if (country == "usa") {
    stop("Calculations for country = 'usa' have not yet been implemented")
  } else if (country == "france") {
    stop("Calculations for country = 'france' have not yet been implemented")
  }



  # Go plot by plot.
  icount = 0
  sp <- NULL
  for (i in 1:nrow(a)) {

    # Progress bar.
    icount <- icount +
      1
    if (verbose) setTxtProgressBar(pb, icount)


    x$species[[i]] <- list(seedlings = character(),
                           saplings = character(),
                           trees = character())

    b <- a[i, ]

    if (country == "spain") {

      seedlings <- if (length(b$trees[[1]]$seedlings) > 0) unique(b$trees[[1]]$seedlings$species) else character()
      saplings <- if (length(b$trees[[1]]$saplings) > 0) unique(b$trees[[1]]$saplings$species) else character()

      # If "individual", there should be a column named "species", even for adult trees.
      # If "ipm", elements in the "trees" list should correspond to species.
      # "mpm" has not been implemented yet.

      if (length(b$trees[[1]]) > 0) {
        if (b$stand_type == "individual") {
          trees <- unique(b$trees[[1]]$species)
        } else if (b$stand_type == "ipm") {
          trees <- names(b$trees[[1]])
        }
      } else trees <- character()




      # Output.
      x$species[[i]] <- mget(c("seedlings", "saplings", "trees"))
      x$species_all[[i]] <- unique(c(seedlings, saplings, trees))
      sp <- c(sp, x$species_all[[i]])
      x$nspecies[i] <- length(x$species_all[[i]])

    }
  }




  # Store the name of the species in 'x'.
  attr(x, "species") <- unique(sp)


  # Extra carriage return.
  if (verbose) cat("\n")


  return(x)

}
