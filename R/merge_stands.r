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

  if (sf::st_crs(a) != sf::st_crs(b)) stop("CRS of inputs 'a' and 'b' do not match")

  if (length(intersect(a$idplot, b$idplot)) > 0) stop("There are plots in both stands with common 'idplot' identifier")

  # Previous copy of attributes.
  a_country <- attr(a, "country")
  b_country <- attr(b, "country")
  a_version <- attr(a, "version")
  b_version <- attr(b, "version")

  # Merging.
  a <- rbind(a, b)

  # Recover attributes that may have been lost during 'rbind'.
  if (!is.null(a_country) | !is.null(b_country)) {
    if (a_country != b_country) stop("Attributes 'country' differ")
    attr(a, "country") <- a_country
  }
  if (!is.null(a_version) | !is.null(b_version)) {
    if (a_version != b_version) stop("Attributes 'version' differ")
    attr(a, "version") <- a_version
  }

  return(a)
}
