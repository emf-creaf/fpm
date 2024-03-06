#' Extract species names of \code{sf} stand object.
#'
#' @description
#' Extract names of all species (i.e. for Spain, seedlings, saplings and trees) per plot and stores their
#' names in a new field. It also adds a new parameter to 'a' called "species" containing
#' the names of all species present.
#'
#' @param a \code{sf} object containing a number of POINT geometry types
#' @param verbose logical, if set to TRUE a progress bar will be printed on screen.
#'
#' @details
#' Simple counting of species per plot.
#'
#' @return
#' Same \code{sf} stand object with three additional fields. For parameter \code{country}="spain",
#' the first field, named "species", includes a list with three elements, "trees", "seedlings" and "saplings".
#' Each one of these
#' elements contains the corresponding species names per plot. The second field, named "species_all",
#' will be a character vector containing the names of all species in that particular plot, irrespective
#' of whether they correspond to trees, seedlings or saplings. The third field, named "nspecies", will
#' be a single number with the total number of species of trees, seedlings or saplings, present in that
#' plot. Therefore, this last field is simply the result of applying function \code{length} to "species_all".
#' A new parameter (aka attribute) is added to 'a' containing the names of all species present.
#'
#' Cases other than \code{country}="spain" have not yet been implemented.
#'
#' @export
#'
#' @examples
#' a <- start_stands()
#' max_dbh <- list('Pinus halepensis' = 200, 'Pinus nigra' = 230)
#' a <- set_parameters(a, param = list(max_dbh = max_dbh, crs =  "EPSG:4326"))
#'
#' # Next, we add one stand.
#' df <- data.frame(species = c('Pinus halepensis', 'Quercus ilex'), dbh = c(8.6, 12.7))
#' a <- build_stand(a, "id1", data = list(df = df), verbose = T)
#'
#' # Calculate species.
#' a <- get_species(a)
#'
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


  # Add new fields to 'a'.
  if (country == "spain") {
    a$species <- a$species_all <- vector("list", length(a$idplot))
    a$nspecies <- 0
  } else if (country == "usa") {
    stop("Calculations for country = 'usa' have not yet been implemented")
  } else if (country == "france") {
    stop("Calculations for country = 'france' have not yet been implemented")
  }


  # Little function used below. Get unique species from column "species" if it is not empty.
  ff <- function(z) if (length(z) > 0) unique(z$species) else character()

  # Go plot by plot.
  icount = 0
  sp <- NULL
  for (i in 1:nrow(a)) {

    # Progress bar.
    icount <- icount + 1
    if (verbose) setTxtProgressBar(pb, icount)


    a$species[[i]] <- list(seedlings = character(),
                           saplings = character(),
                           trees = character())

    b <- a[i, ]

    if (country == "spain") {


      seedlings <- ff(b$seedlings[[1]])
      saplings <- ff(b$saplings[[1]])


      # If "individual", there must be a column named "species", even for adult trees.
      # If "ipm", elements in the "trees" list should correspond to species.
      if (length(b$trees[[1]]) > 0) {
        if (b$stand_type == "individual") {
          trees <- unique(b$trees[[1]]$species)
        } else if (b$stand_type == "ipm") {
          trees <- names(b$trees[[1]])
        }
      } else trees <- character()


      # Output.
      a$species[[i]] <- mget(c("seedlings", "saplings", "trees"))
      a$species_all[[i]] <- unique(c(seedlings, saplings, trees))
      sp <- c(sp, a$species_all[[i]])
      a$nspecies[i] <- length(a$species_all[[i]])

    }
  }


  # Store the name of the species in 'x'.
  attr(a, "species") <- unique(sp)


  # Extra carriage return.
  if (verbose) cat("\n")


  return(a)

}
