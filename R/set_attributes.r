#' Set attributes of a \code{sf} tree-stand object
#'
#' @description
#' Set attributes in a \code{sf} object that will be required during
#' calculations of tree-stand properties
#'
#' @param a a \code{sf} tree-stand object created with \code{\link{start_stand}}.
#' @param country string indicating which country the stand belongs to.
#' At this moment, "Spain" (default), "France" or "USA" are valid inputs, though
#' only calculations for the Spanish Inventario Forestal Nacional have been
#' implemented. Lower or upper case letters can be used. No default value is set.
#' @param integvars Optional data frame containing, in columns, the abscissas for each
#' species to be used by the IPM methodology. Future implementations will likely
#' allow the use of different number of abscissas for each species, but at this
#' moment this is not permitted.
#' @param min_dbhnamed vector containing the minimum dbh after which a tree will
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
#' a <- start_stand("ID1", 5, 45, "EPSG:4326")
#' max_dbh <- c('Pinus halepensis' = 200, 'Pinus nigra' = 230)
#' a <- set_attributes(a, max_dbh = max_dbh)
#'
#'
#'
set_attributes <- function(a, country = NULL, integvars = NULL, min_dbh = NULL, max_dbh = NULL) {

  if (is.null(country) & is.null(integvars) & is.null(min_dbh) & is.null(max_dbh)) warning("No attribute has been set!")

  if (!is.null(country)) {
    attr(a, "country") <- match.arg(tolower(country), choices = c("spain", "usa", "france"))
  }

  if (!is.null(integvars)) {
    stopifnot(is.data.frame(integvars))
    attr(a, "integvars") <- integvars
  }

  if (!is.null(min_dbh)) {
    stopifnot(is.data.frame(min_dbh))
    attr(a, "min_dbh") <- min_dbh
  }

  if (!is.null(max_dbh)) {
    stopifnot(is.data.frame(max_dbh))
    attr(a, "max_dbh") <- max_dbh
  }

  return(a)
}
