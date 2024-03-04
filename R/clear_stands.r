#' Clear \code{sf} stand object of unneeded fields.
#'
#' @description
#' It resets fields in the input \code{sf} stand object to an empty fields, and remove any additional field.
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
  stopifnot("Input 'a' is empty" = nrow(a) > 0)


  # Which country?
  country <- match.arg(get_parameters(a, "country")$country, c("spain", "france", "usa"))


  if (country == "spain") {
    # Resetting fields to an empty list.
    a$seedlings <- a$saplings <- a$trees <- vector("list", nrow(a))

    # Removing fields.
    a$species <- a$species_all <- a$nspecies <- a$ba <- a$ba_species <- a$ntrees <- a$ntrees_species <- NULL

  }  else if (country == "usa") {
    stop("Calculations for country = 'usa' have not yet been implemented")
  } else if (country == "france") {
    stop("Calculations for country = 'france' have not yet been implemented")
  }


  return(a)

}
