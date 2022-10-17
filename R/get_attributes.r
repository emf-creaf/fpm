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

    abscissas <- attr(a, "abscissas")
    type <- attr(a, "type")
    country <- attr(a, "country")

    return(c(abscissas=g(abscissas),
             type=g(type),
             country=g(country)))


}
