#' Title
#'
#' @param a
#'
#' @return
#' @export
#'
#' @examples
get_attributes <- function(a) {

  country <- attr(a, "country")
  integration_variable <- attr(a, "integration_variable")

  return(c(country=null_to_NA(country),
           integration_variable=null_to_NA(integration_variable)))
}
