#' Title
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
start_stand <- function(idplot, x, y, stand_type = "individual", date = NULL, crs = NULL) {

  mf <- match.call()
  m <- match(c("idplot", "x", "y","stand_type", "date", "crs"), tolower(names(mf)))
  if (any(is.na(m[1:5]))) stop("Missing 'idplot', 'x', 'y', 'stand_type' or 'date'")

  if (!any(stand_type %in% c("individual", "mpm", "ipm"))) stop("Wrong 'stand_type' input")
  if (is.null(crs)) {
    crs <- NA
  } else {
    if (length(crs) > 1) warning("Input 'crs' should not be a vector. Using its first element only...")
  }
  if (any(sapply(list(idplot, x, y, stand_type),length) != 1)) {
    stop("Length of inputs 'idplot', 'x', 'y' and 'stand_type' cannot be > 1")
  }
  if (!is.null(date)) {
    if (length(date) > 1) stop("Input 'date' must be a single number")
  }

  # Start sf object.
  n <- length(idplot)
  geometry <- setNames(sf::st_point(c(x,y), dim="XY"), idplot)
  z <- sf::st_sf(idplot=idplot, geometry = sf::st_sfc(geometry))
  sf::st_crs(z) <- crs

  # Initialize years (i.e. sampling date) and stand type.
  z$date <- date
  z$stand_type <- stand_type

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
