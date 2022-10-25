#' Title
#'
#' @param a
#'
#' @return
#' @export
#'
#' @examples
get_attributes <- function(a) {

  if (!any(class(a) %in% "inventory")) stop("Object 'a' must be of 'inventory' class")

  g <- function(x) ifelse(is.null(x), NA, x)

  country <- attr(a, "country")
  version <- attr(a, "version")

  return(c(country=g(country),
           version=g(version)))
}
