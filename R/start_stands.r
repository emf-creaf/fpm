#' Start \code{sf} object to represent a tree stand
#'
#' @param control
#'
#' @description
#' Make the basic structure of a \code{sf} object to contain stand data.
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
start_stands <- function(control = list()) {


  # Start sf object with dummy coordinates.
  z <- sf::st_as_sf(data.frame(X = 0, Y = 0), coords = c("X", "Y"))


  # Check parameters.
  if (length(control) > 0) {
    namc <- names(control)
    i <- namc %in% c("country", "integvars", "min_dbh", "max_dbh", "crs")
    stopifnot("Wrong parameter names" = any(i))
    z <- z |> set_parameters(country = control[["country"]],
                             integvars = control[["integvars"]],
                             min_dbh = control[["min_dbh"]],
                             max_dbh = control[["max_dbh"]],
                             crs = control[["crs"]])
  }


  # Empty identifier column.
  z$idplot <- ""


  # Empty date column.
  z$date <- ""


  # Stand type is character.
  z$stand_type <- ""


  # Empty lists that will contain seedlings, saplings and trees.
  z$seedlings <- z$saplings <- z$trees <- vector("list", 1)


  return(z)
}
