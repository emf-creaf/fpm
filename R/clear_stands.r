#' Create a new \code{sf} stand object.
#'
#' @description
#' It creates a new \code{sf} stand object that is identical to the input, but
#' with fields empty or NULL, save for "idplot" and "geometry", which are kept.
#' Attributes, on the other hand, are also kept.
#'
#' @param a a \code{sf} object containing a number of POINT geometry types.
#'
#' @return
#' Same input \code{sf} object with the same parameters (aka attributes) but
#' with fields removed or emptied.
#'
#' @export
#'
#' @details
#' Additional details...
#'
#'
#' @examples
#' # First initialize stands.
#' a <- start_stands()
#' maxdbh <- list('Pinus halepensis' = 200, 'Pinus nigra' = 230)
#' a <- set_parameters(a, param = list(maxdbh = maxdbh, crs =  "EPSG:4326"))
#'
#' # Next, we add one stand.
#' df <- data.frame(species = c('Pinus halepensis', 'Quercus ilex'), dbh = c(8.6, 12.7))
#' a <- build_stand(a, "id1", data = list(df = df), verbose = T)
#'
#' b <- clear_stands(a)
clear_stands <- function(a) {

  # First checks.
  stopifnot("Input 'a' must be an sf object" = inherits(a, "sf"))


  # Which country?
  country <- match.arg(get_parameters(a, "country")$country, c("spain", "france", "usa"))
  if (country == "usa") {
    stop("Calculations for country = 'usa' have not yet been implemented")
  } else if (country == "france") {
    stop("Calculations for country = 'france' have not yet been implemented")
  }


  # Resetting fields.
  names_a <- names(a)
  start_a <- start_stands()
  flag <- names_a %in% names(start_a)

  # Eliminate fields that are not in start_stands.
  for (i in which(!flag)) {
    a[, names_a[i]] <- NULL
  }


  # Empty fields that are in start_stands(), save for "idplot" and "geometry".
  a$stand_type <- ""
  a$date <- ""
  for (i in 1:nrow(a)) a[i, ]$seedlings[[1]] <- a[i, ]$saplings[[1]] <- a[i, ]$trees[[1]] <- list()


  return(a)

}
