#' Title
#'
#' @param a
#' @param idplot
#' @param type
#' @param width
#'
#' @return
#' @export
#'
#' @examples
smooth_stand <- function(a, idplot, type = "gaussian", width = 2) {

  if (length(idplot) > 1) stop("Input 'idplot' must have length = 1")
  i <- match(idplot, a$idplot)
  if (is.na(i)) stop(cat("Stand",idplot,"does not exist"))

  df <- 1


}
