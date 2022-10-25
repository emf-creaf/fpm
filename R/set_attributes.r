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
set_attributes <- function(a, country = NULL, version = NULL, x = NULL) {

  if (!any(class(a) %in% "inventory")) stop("Object 'a' must be of 'inventory' class")

  if (!is.null(country)) {
    if (!any(tolower(country) %in% c("spain", "usa", "france"))) stop("Wrong 'country' value")
    attr(a, "country") <- country
  }

  if (!is.null(version)) {
    if (!any(version %in% c(2,3,4))) stop("Wrong 'version' value")
    attr(a, "version") <- version
  }

  if (!is.null(x)) {
    if (!(is.data.frame(x) | is.matrix(x))) stop("Input 'x' must be a data.frame or a matrix with as many columns as species")
    attr(a, "integration_variable") <- x
  }

  return(a)
}
