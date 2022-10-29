#' Title
#'
#' @param a
#' @param idplot
#' @param b
#'
#' @return
#' @export
#'
#' @examples
add_stand <- function(a, b) {

  if (sf::st_crs(a) != sf::st_crs(b)) stop("CRS of inputs 'a' and 'b' do not match")
  if (length(idplot) > 1) stop("Input 'idplot' must have length = 1")
  a_country <- tolower(attr(a, "country"))
  if (a_country != tolower(attr(b, "country"))) stop("Attribute 'country' in 'a' and 'b' must match")
  a_version <- attr(a, "version")
  # if (a_version != attr(b, "version")) stop("Attribute 'version' in 'a' and 'b' must match")
  # a_integration_variable <- attr(a, "integration_variable")

  i <- match(idplot, a$idplot)
  if (is.na(i)) {
    a <- rbind(a, b)
  } else {
    a[i, ] <- b
  }

  # Some attributes are lost during 'rbind'.
  if (!is.null(a_country)) attr(a, "country") <- a_country


  return(a)
}
