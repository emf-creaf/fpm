#' Start \code{sf} object to represent a tree stand
#'
#' @description
#' Make the basic structure of a \code{sf} object to contain stand data.
#'
#' @param idplot numeric/character vector, unique identifier of tree stands.
#' @param x geographic coordinate of stands (e.g. longitude if unprojected,
#' X in m if UTM projected)
#' @param y geographic coordinate of stand (same as for \code{x}).
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
start_stand <- function(idplot, x, y, crs = NA) {

  mf <- match.call()
  m <- match(c("idplot", "x", "y", "crs"), tolower(names(mf)))
  if (any(is.na(m[1:3]))) stop("Missing 'idplot', 'x' or 'y'")

  if (!is.vector(idplot) | !is.vector(x) | !is.vector(y))
    stop("Inputs 'idplot', 'x' and 'y' must be vectors")

  if (length(unique(c(length(idplot), length(x), length(y)))) != 1)
    stop("Length of 'idplot', 'x' and 'y' must be equal")

  # Start sf object.
  z <- sf::st_as_sf(data.frame(X=x, Y=y), coords = c("X", "Y"), crs = crs)
  z$idplot <- idplot

  # Initialize two columns that refer to type of data and type of stand.
  z$stand_type = NA
  z$date <- NA

  # To contain character vector with species present in this stand.
  z$species[[1]] <- list()

  # Initialize trees and saplings.
  z$trees[[1]] <- list()
  z$seedlings[[1]] <- list()
  z$saplings[[1]] <- list()

  # Aggregated info.
  z$N_species[[1]] <- list()
  z$BA_species[[1]] <- list()
  z$N_stand <- NA                       # Number of tree species in stand. A single number.
  z$BA_stand <- NA                      # Total basal area in stand. A single number.

  return(z)
}
