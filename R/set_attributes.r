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
#' a <- start_inventory(letters[1:5],runif(5),runif(5),rep("individual",5),runif(5),"EPSG:4326")
#' a <- set_attributes(a)
set_attributes <- function(a, country = NULL, x = NULL) {

  if (!is.null(country)) {
    if (!any(tolower(country) %in% c("spain", "usa", "france"))) stop("Wrong 'country' value")
    attr(a, "country") <- country
  }

  if (!is.null(x)) {
    if (!(is.data.frame(x))) stop("Input 'x' must be a data.frame with as many columns as species")
    attr(a, "integration_variable") <- x
  }

  return(a)
}
