#' Extract species names
#'
#' @description
#' Extract names of all species (i.e. seedlings, saplings, trees) per plot.
#'
#' @param a
#' @param verbose
#'
#' @return
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
    cat(paste0("\n -> ", fname, ": Creating new 'sf' with species names per plot...\n"))
    pb <- txtProgressBar(min = 0,
                         max = length(nrow(a)),
                         style = 3,
                         width = 50,
                         char = "=")
  }


  # Create a new 'sf' object to store the names of the species per plot.
  x <- a
  if (attr(x, "country") == "spain") {
    x$trees <- x$seedlings <- x$saplings <- NULL
    x$species <- x$species_all <- vector("list", length(x$idplot))
    x$nspecies <- 0
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
      } else if (b$stand_type == "mpm") {
        stop("Not implemented yet")
      }
    } else trees <- character()




    # Output.
    x$species[[i]] <- mget(c("seedlings", "saplings", "trees"))
    x$species_all[[i]] <- unique(c(seedlings, saplings, trees))
    sp <- c(sp, x$species_all[[i]])
    x$nspecies[i] <- length(x$species_all[[i]])
  }




  # Store the name of the species in 'x'.
  attr(x, "species") <- unique(sp)


  # Extra carriage return.
  if (verbose) cat("\n")


  return(x)

}
