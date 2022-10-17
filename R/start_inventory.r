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
#' a <- start_inventory(letters[1:5],runif(5), runif(5),"EPSG:4326")
#'
start_inventory <- function(idplot, x, y, crs = NULL) {

  mf <- match.call()
  m <- match(c("idplot", "x", "y", "crs", "type"), tolower(names(mf)))
  if (any(is.na(m[1:3]))) stop("Missing 'idplot', 'x' or 'y'")
  n <- length(idplot)
  if ((n != length(y)) | (n != length(y))) stop("Lengths of 'idplot', 'x' and 'y' must be equal")
  if (is.null(crs)) crs <- NA

  # sf object.
  geometry <- setNames(lapply(1:n, function(i) sf::st_point(c(x[i],y[i]), dim="XY")), idplot)
  z <- sf::st_sf(idplot=idplot, geometry = sf::st_sfc(geometry))
  sf::st_crs(z) <- crs

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
