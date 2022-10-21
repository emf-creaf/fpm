#' Title
#'
#' Initialization of a 'sf' object.
#'
#' @param idplot
#' @param x
#' @param y
#' @param crs
#' @param type
#'
#' @return
#'
#' @export
#'
#' @examples
#' a <- start_inventory(letters[1:5],runif(5),runif(5),rep("individual",5),runif(5),"EPSG:4326")
#'
start_inventory <- function(idplot, x, y, stand_type = NULL, date = NULL, crs = NULL) {

  mf <- match.call()
  m <- match(c("idplot", "x", "y","stand_type", "date", "crs"), tolower(names(mf)))
  if (any(is.na(m[1:4]))) stop("Missing 'idplot', 'x', 'y', 'stand_type' or 'date'")
  if (!any(stand_type %in% c("individual", "mpm", "ipm"))) stop("Wrong 'stand_type' input")
  if (is.null(crs)) {
    crs <- NA
  } else {
    if (length(crs) > 1) warning("Input 'crs' should not be a vector. Using its first element only...")
  }
  if (var(sapply(list(idplot, x, y, stand_type, date),length))>0)
    stop("Inputs vectors 'idplot', 'x', 'y', stand_type and 'date' must all have the same length")

  # Start sf object.
  n <- length(idplot)
  geometry <- setNames(lapply(1:n, function(i) sf::st_point(c(x[i],y[i]), dim="XY")), idplot)
  z <- sf::st_sf(idplot=idplot, geometry = sf::st_sfc(geometry))
  sf::st_crs(z) <- crs

  # Initialize years (i.e. sampling date) and stand type.
  z$date <- date
  z$stand_type <- stand_type
  # Initialize trees and saplings.
  z$trees <- vector("list", n)          # Number of trees per diametric class and species. A three-column matrix.
  z$saplings <- vector("list", n)       # Number of saplings per species. A two-column matrix.

  # Aggregated info.
  z$N_species <- vector("list", n)      # Number of tree individuals per species. A two-column matrix.
  z$BA_species <- vector("list", n)     # Basal area per tree species. A two-column matrix.
  z$species <- vector("list", n)        # Tree species present in stand. A two-column matrix.
  z$N_stand <- NA                       # Number of tree species in stand. A single number.
  z$BA_stand <- NA                      # Total basal area in stand. A single number.

  # Append new class "inventory".
  class(z) <- append(class(z), values = "inventory", after = 0)

  return(z)
}
