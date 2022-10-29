#' Title
#'
#' @description
#' Make the basic structure of a 'sf' object to contain stand data.
#'
#' @param idplot
#' @param x
#' @param y
#' @param stand_type
#' @param date
#' @param crs
#'
#' @return
#' @export
#'
#' @examples
start_stand <- function(idplot, x, y, crs = NULL) {

  mf <- match.call()
  m <- match(c("idplot", "x", "y", "crs"), tolower(names(mf)))
  if (any(is.na(m[1:3]))) stop("Missing 'idplot', 'x' or 'y'")
  if (!is.null(crs)) {
    if (length(crs) > 1) warning("Input 'crs' should not be a vector. Using its first element only...")
  } else {
    crs <- fpm:::null_to_NA(crs)
  }
  if (any(sapply(list(idplot, x, y),length) != 1)) {
    stop("Length of inputs 'idplot', 'x' or 'y' cannot be > 1")
  }

  # Start sf object.
  n <- length(idplot)
  geometry <- setNames(sf::st_point(c(x,y), dim="XY"), idplot)
  z <- sf::st_sf(idplot=idplot, geometry = sf::st_sfc(geometry))
  sf::st_crs(z) <- crs

  # Initialize trees and saplings.
  z$trees[[1]] <- list()
  z$seedlings[[1]] <- vector(max(fpm:::num_seedlings()), mode = "list")
  z$saplings[[1]] <- vector(max(fpm:::num_saplings()), mode = "list")

  # Aggregated info.
  z$N_species[[1]] <- list()
  z$BA_species[[1]] <- list()
  z$species[[1]] <- list()
  z$N_stand <- NA                       # Number of tree species in stand. A single number.
  z$BA_stand <- NA                      # Total basal area in stand. A single number.

  return(z)
}
