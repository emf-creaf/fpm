#' Title
#'
#' @param a
#' @param idplot
#'
#' @return
#' @export
#'
#' @examples
#'
show_stand <- function(a, idplot) {

  if (!any(class(a) == "sf")) stop("Input 'a' is not a 'sf' object")
  id <- match(idplot, a$idplot)
  if (is.na(id)) stop("Could not find 'idplot' in 'a'")
  if (length(id) != 1) stop("Only one 'idplot' can be modified at the time")

  return(a[id, ])

}
