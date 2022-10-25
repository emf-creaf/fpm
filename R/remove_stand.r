#' Title
#'
#' @param a
#' @param idplot
#'
#' @return
#' @export
#'
#' @examples
remove_stand <- function(a, idplot) {

  if (!any(class(a) %in% "inventory")) stop("Object 'a' must be of 'inventory' class")

  if (length(idplot) > 1) stop("Input 'idplot' must have length = 1")
  i <- match(idplot, a$idplot)
  if (is.na(i)) stop(cat("Stand",idplot,"does not exist"))

  return(a[-i,])


}
