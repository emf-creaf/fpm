#' Start \code{sf} object to represent a tree stand
#'
#' @description
#' Make the basic structure of a \code{sf} object to contain stand data.
#'
#' @param idplot unique identifier of tree stand.
#' @param x geographic coordinate of stand (e.g. longitude if unprojected, X in m if UTM projected)
#' @param y geographic coordinate of stand (same as for \code{x.
#' @param crs coordinate reference system of stand. If missing, NA will be assumed.
#'
#' @details This simple function creates a simple feature (a.k.a. \code{sf}) POINT
#' object to store all data for a single tree stand.
#'
#' @return
#' A \code{sf} object with new fields.
#'
#' @export
#'
#' @examples
#' a <- start_stand("ID1", 5, 45, "EPSG:4326")
#'
start_stand <- function(idplot, x, y, crs) {

  mf <- match.call()
  m <- match(c("idplot", "x", "y", "crs"), tolower(names(mf)))
  if (any(is.na(m[1:3]))) stop("Missing 'idplot', 'x' or 'y'")
  if (any(sapply(list(idplot, x, y),length) != 1)) {
    stop("Length of inputs 'idplot', 'x' or 'y' cannot be > 1")
  }

  # Start sf object.
  n <- length(idplot)
  geometry <- setNames(sf::st_point(c(x,y), dim="XY"), idplot)
  z <- sf::st_sf(idplot=idplot, geometry = sf::st_sfc(geometry))

  # Coordinate reference system.

  if (!is.null(m[4])) sf::st_crs(z) <- crs

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
