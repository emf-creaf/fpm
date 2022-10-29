#' Title
#'
#' @description
#' Remove plots
#'
#' @param a
#' @param idplot
#'
#' @return
#' @export
#'
#' @examples
remove_stand <- function(a, idplot) {

  i <- match(idplot, a$idplot)
  if (any(is.na(i))) stop(cat("Could not find some plots"))

  return(a[-i,])

}
