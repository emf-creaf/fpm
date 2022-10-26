#' Title
#'
#' @param a
#'
#' @return
#' @export
#'
#' @examples
get_integvar <- function(a) {
  return(attr(a, "integration_variable"))
}
