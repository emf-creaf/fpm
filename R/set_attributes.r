#' Title
#'
#' @param a
#' @param x
#' @param type
#'
#' @return
#'
#' @details
#' 'Spain': Spanish Inventario Forestal Nacional data.
#' 'USDA': USDA Forest Inventory Analysis data.
#' 'France': French Inventaire Forestier National.
#'
#'
#' @export
#'
#' @examples
set_attributes <- function(a, abscissas = NULL, type = NULL, country = NULL) {

  if (!is.null(type)) {
    if (!any(type %in% c("individual", "matrix", "continuous"))) stop("Wrong 'type' value")
    attr(a, "type") <- type
  }

  if (!is.null(country)) {
    if (!any(tolower(country) %in% c("spain", "usa", "france"))) stop("Wrong 'country' value")
    attr(a, "country") <- "Spain"
  }

  if (!is.null(abscissas)) {
    attr(a, "abscissae") <- abscissas
  }

  return(a)
}
