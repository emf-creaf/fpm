#' Title
#'
#' @param a
#'
#' @return
#' @export
#'
#' @examples
clear_stands <- function(a) {

  # First checks.
  stopifnot("Input 'a' must be an sf object" = inherits(a, "sf"))

  if (nrow(a) == 0) stop("Input 'a' is empty")

  for (i in 1:length(a$idplot)) {
    a$seedlings <- a$saplings <- a$trees <- vector("list", length(a$idplot))
  }

  return(a)

}
