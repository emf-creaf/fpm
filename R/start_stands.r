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
#' A \code{sf} object with new empty fields and attributes.
#'
#' @export
#'
#' @examples
#' a <- start_stands()
#'
start_stands <- function(idplot = idplot, crs = NA) {


  # Start sf object with dummy coordinates.
  z <- sf::st_as_sf(data.frame(X = 0, Y = 0), coords = c("X", "Y"), crs = crs)
  z$idplot <- idplot


  # Empty date column. Initially it's an empty character.
  z$date <- ""


  # Stand type is character.
  z$stand_type <- ""


  # Empty lists that will contain seedlings, saplings and trees.
  z$seedlings <- z$saplings <- z$trees <- vector("list", length(idplot))


  return(z)
}
