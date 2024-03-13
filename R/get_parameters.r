#' Extract parameters from \code{stand} object
#'
#' @description
#' Extraction of global parameters from \code{stand} object
#'
#'
#' @param a \code{sf} object containing a number of POINT geometry types
#' @param param character vector with names of parameters to be extracted:
#' \code{country}, \code{integvars}, \code{h}, \code{min_dbh}, \code{max_dbh},
#' \code{species} or \code{crs}. If not provided, all those parameters will
#' be retrieved.
#'
#' @return
#' A \code{list} whose elements correspond to the input parameters. Parameter
#' \code{species} can only be computed via \code{\link{calc_species}} function.
#'
#' @export
#'
#' @details
#' See \code{\link{start_stand}} and \code{\link{set_parameters}} for details.
#'
#' @examples
#' a <- start_stands()
#' max_dbh <- list('Pinus halepensis' = 200, 'Pinus nigra' = 230)
#' a <- set_parameters(a, param = list(max_dbh = max_dbh, crs =  "EPSG:4326"))
#' p <- get_parameters(a)
#'
get_parameters <- function(a, param = NULL) {

  # Must be an "sf" object.
  stopifnot("Input 'a' must be an 'sf' object" = any(class(a) == "sf"))


  # Check that parameters are correct. If NULL, all parameters.
  par <- c("country", "integvars", "h", "min_dbh", "max_dbh", "crs", "species")

  if (is.null(param)) {
    param <- par

  } else {
    # It should be a vector.
    stopifnot("Input 'parameters' must be a character vector" = is.vector(param))

    # Check that parameters are correct.
    stopifnot("Incorrect 'param' names" = all(param %in% par))

  }


  # Extract parameters.
  p <- sapply(param,
              function(x) {
                if (x == "crs") {
                  sf::st_crs(a)
                } else {
                  attr(a, x)
                }
              },
              simplify = F)


  # Check that 'country', if present, is correct.
  if ("country" %in% param) p$country <- match.arg(attr(a, "country"), choices = c("spain", "usa", "france"))


  return(p)
}

