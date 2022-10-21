#' Title
#'
#' @param a
#'
#' @return
#' @export
#'
#' @examples
get_attributes <- function(a) {

    g <- function(x) ifelse(is.null(x), NA, x)

    country <- attr(a, "country")
    abscissas <- attr(a, "abscissas")

    return(c(abscissas=g(abscissas),
             country=g(country)))


}
