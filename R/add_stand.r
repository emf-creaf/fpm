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
add_stand <- function(a, idplot, b) {

  if (sf::st_crs(a) != sf::st_crs(b)) stop("CRS of inputs 'a' and 'b' do not match")
  if (length(idplot) > 1) stop("Input 'idplot' must have length = 1")
  a_country <- tolower(attr(a, "country"))
  if (a_country != tolower(attr(b, "country"))) stop("Attribute 'country' in 'a' and 'b' must match")
  i <- match(idplot, a$idplot)

  if (is.na(i)) {
    a <- rbind(a, b)
  } else {
    a[i, ] <- b
  }

  # Attribute "country" is lost during 'rbind'.
  attr(a, "country") <- a_country

  return(a)
}
