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
#' a <- start_stands(c("ID1", "ID2"), c(4, 5), c(45, 45), "EPSG:4326")
#'
start_stands <- function(idplot, x, y, crs = NA) {

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


  # Empty date column. Initially it's an empty character.
  z$date <- ""


  # Stand type is character.
  z$stand_type <- ""


  # Empty lists that will contain seedlings, saplings and trees.
  z$seedlings <- z$saplings <- z$trees <- vector("list", length(idplot))


  return(z)
}
