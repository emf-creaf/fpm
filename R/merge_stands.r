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

  # Attributes before 'rbind'.
  a_country <- attr(a, "country")
  b_country <- attr(b, "country")
  a_integvars <- attr(a, "integvars")
  b_integvars <- attr(b, "integvars")


  # 'country' and 'integvars' attributes must match, if already set.
  if (!is.null(a_country) | !is.null(b_country)) {
    if (a_country != b_country) stop("Attributes 'country' differ")
  }
  if (!is.null(a_integvars) | !is.null(b_integvars)) {
    if (!identical(a_integvars, b_integvars)) stop("Attributes 'integvars' differ")
  }

  # Merging.
  ab <- rbind(a, b)
  if (!is.null(a_country)) attr(ab, "country") <- a_country
  if (!is.null(a_integvars)) attr(ab, "integvars") <- a_integvars

  return(ab)
}
