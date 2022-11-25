#' Title
#'
#' @param a
#' @param idplot
#'
#' @return
#' @export
#'
#' @examples
check_exist <- function(a, idplot) {

  i <- match(idplot, a$idplot)
  ifelse(is.na(i), FALSE, TRUE)

}
