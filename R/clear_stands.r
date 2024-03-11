#' Create a new \code{sf} stand object.
#'
#' @description
#' It creates a new \code{sf} stand object that is identical to the input, but
#' with fields empty or NULL. Attributes, on the other hand, are kept.
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
#' max_dbh <- list('Pinus halepensis' = 200, 'Pinus nigra' = 230)
#' a <- set_parameters(a, param = list(max_dbh = max_dbh, crs =  "EPSG:4326"))
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


  if (country == "spain") {
    # Resetting fields to an empty list.
    b <- start_stands()
    attributes(b) <- attributes(a)
    sf::st_crs(b) <- sf::st_crs(a)

  }  else if (country == "usa") {
    stop("Calculations for country = 'usa' have not yet been implemented")
  } else if (country == "france") {
    stop("Calculations for country = 'france' have not yet been implemented")
  }


  return(b)

}
