#' Merge tree stands
#'
#' @description
#' Merge \code{sf} objects representing tree-stands
#'
#' @param a first tree-stand \code{sf} object to merge.
#' @param b second tree-stand \code{sf} object to merge.
#'
#' @return
#' A new tree-stand \code{sf} containing both all stands in \code{a} and \code{b}.
#'
#' @details Attributes in stands \code{a} and \code{b} must match for \code{merge_stand}
#' to work. The resulting \code{sf} object will have the same attributes as both
#' inputs.
#'
#' @export
#'
#' @examples
#'
#' #' # First initialize one single stand.
#' a <- start_stand("ID1", 5, 45, "EPSG:4326")
#' a <- set_attributes(a, country = "spain")
#'
#' # Next, we merge other stands.
#' for (i in 2:10) {
#' b <- start_stand(paste0("ID",i), 5, 45, "EPSG:4326")
#' b <- set_attributes(b, country = "spain")
#' a <- merge_stands(a,b)
#' }
#'
merge_stands <- function(a, b) {

  if (sf::st_crs(a) != sf::st_crs(b)) stop("CRS of inputs 'a' and 'b' do not match")
  if (length(intersect(a$idplot, b$idplot)) > 0) stop("There are plots in both stands with common 'idplot' identifier")

  # Attributes before 'rbind'.
  a_country <- attr(a, "country")
  b_country <- attr(b, "country")
  a_integvars <- attr(a, "integvars")
  b_integvars <- attr(b, "integvars")

  # 'country' and 'integvars' attributes must match, if already set.
  if (!is.null(a_country) & !is.null(b_country)) {
    if (a_country != b_country) stop("Attributes 'country' differ")
  } else{
    stop("Attribute 'country' must be set")
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
