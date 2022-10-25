#' Title
#'
#' @param a
#' @param b
#'
#' @return
#' @export
#'
#' @examples
merge_stands <- function(a, b) {

  if (!any(class(a) %in% "inventory") | !any(class(b) %in% "inventory")) stop("Object 'a' must be of 'inventory' class")

  if (sf::st_crs(a) != sf::st_crs(b)) stop("CRS of inputs 'a' and 'b' do not match")

  a <- rbind(a, b)

  # Some attributes are lost during 'rbind'.
  a_country <- attr(a, "country")
  a_version <- attr(a, "version")
  if (!is.null(a_country)) attr(a, "country") <- a_country
  if (!is.null(a_version)) attr(a, "version") <- a_version

  return(a)
}
