#' Set parameters of a \code{sf} \code{stand} object
#'
#' @description
#' Set parameters in a \code{sf} object that will be required during
#' calculations of stand properties
#'
#' @param a a \code{sf} stand object created with \code{\link{start_stand}}.
#' @param country string indicating which country the stand belongs to.
#' At this moment, "spain" (default), "france" or "usa" are valid inputs, though
#' only calculations for the Spanish Inventario Forestal Nacional have been
#' implemented. Lower or upper case letters can be used. No default value is set.
#' @param integvars optional \code{list} where each named element contains the abscissas for
#' the species to be modeled.
#' @param min_dbh named vector containing the minimum dbh after which a tree will
#' be considered as an adult individual.
#' @param max_dbh named vector containing the maximum dbh reachable for each species.
#'
#' @return
#' A \code{sf} object with attributes \emph{country} and \emph{x} set.
#'
#' @details
#' Attribute \emph{country} must be set to either of the three possible values.
#' The default is "Spain", and in fact it is the only country for which
#' stand dynamics calculations have been implemented at this stage.
#'
#' @export
#'
#' @examples
#' a <- start_stands(c("ID1", "ID2"), c(4, 5), c(45, 45), "EPSG:4326")
#' max_dbh <- list('Pinus halepensis' = 200, 'Pinus nigra' = 230)
#' a <- set_parameters(a, max_dbh = max_dbh)
#'
set_parameters <- function(a, country = NULL, integvars = NULL, min_dbh = NULL, max_dbh = NULL) {

  # Must be an "sf" object.
  stopifnot("Input 'a' must be an 'sf' object" = any(class(a) == "sf"))

  # Any parameter?
  if (is.null(country) & is.null(integvars) & is.null(min_dbh) & is.null(max_dbh)) {
    warning("No attribute has been set!")
    return(a)
  }

  if (!is.null(country)) {
    attr(a, "country") <- match.arg(tolower(country), choices = c("spain", "usa", "france"))
  }

  if (!is.null(integvars)) {
    stopifnot("Input 'integvars' must be a named list" = is.list(integvars))
    attr(a, "integvars") <- integvars
    attr(a, "h") <- sapply(integvars, function(x) x[2]-x[1], simplify = F)
  }

  if (!is.null(min_dbh)) {
    stopifnot("Input 'min_dbh' must be a named list" = is.list(min_dbh))
    attr(a, "min_dbh") <- min_dbh
  }

  if (!is.null(max_dbh)) {
    stopifnot("Input 'max_dbh' must be a named list" = is.list(max_dbh))
    attr(a, "max_dbh") <- max_dbh
  }

  return(a)
}
