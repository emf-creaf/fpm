#' Title
#'
#' @param a
#' @param idplot
#'
#' @return
#' @export
#'
#' @examples
extract_stands <- function(a, idplot) {

  i <- match(idplot, a$idplot)
  if (is.na(i)) stop("Could not find some 'idplot' in a")

  return(a[idplot, ])
}
