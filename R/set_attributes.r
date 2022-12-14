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
#' a <- set_attributes(a)
#'
set_attributes <- function(a, country = NULL, integvars = NULL) {

  if (is.null(country) & is.null(integvars)) warning("No attribute has been set!")

  if (!is.null(country)) {
    attr(a, "country") <- match.arg(tolower(country), choices = c("spain", "usa", "france"))
  }

  if (!is.null(integvars)) {
    if (!(is.data.frame(integvars))) stop("Input 'integvars' must be a data.frame with as many columns as species")
    attr(a, "integvars") <- integvars
  }

  return(a)
}
