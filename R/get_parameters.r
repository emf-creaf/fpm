#' Extract parameters from \code{stand} object
#'
#' @description
#' Extraction of global parameters from \code{stand} object
#'
#'
#' @param a \code{sf} object containing a number of POINT geometry types
#' @param parameters character vector with names of parameters to be extracted:
#' \code{country}, \code{integvars}, \code{h}, \code{min_dbh}, \code{max_dbh},
#' \code{species} or \code{crs}. If not provided, all those parameters will
#' be retrieved.
#'
#' @return
#' A \code{list} whose elements correspond to the input parameters.
#'
#' @export
#'
#' @details
#' See \code{\link{start_stand}} and \code{\link{set_parameters}}for details.
#'
#' @examples
#' a <- start_stands()
#' max_dbh <- list('Pinus halepensis' = 200, 'Pinus nigra' = 230)
#' a <- set_parameters(a, control = list(max_dbh = max_dbh, crs =  "EPSG:4326"))
#' p <- get_parameters(a)
#'
get_parameters <- function(a, parameters = NULL) {

  # Must be an "sf" object.
  stopifnot("Input 'a' must be an 'sf' object" = any(class(a) == "sf"))


  # Check that parameters are correct. If NULL, all parameters.
  param <- c("country", "integvars", "h", "min_dbh", "max_dbh", "crs", "species")

  if (is.null(parameters)) {

    parameters <- param

  } else {
    # It should be a vector.
    stopifnot("Input 'parameters' must be a character vector" = is.vector(parameters))

    # Check that parameters are correct.
    stopifnot("Incorrect 'parameters' names" = all(parameters %in% param))

  }


  # Extract parameters.
  p <- sapply(parameters,
              function(x) {
                if (x == "crs") {
                  sf::st_crs(a)
                } else {
                  attr(a, x)
                }
              },
              simplify = F)
  if (length(parameters) == 1) p <- p[[1]]


  return(p)
}

