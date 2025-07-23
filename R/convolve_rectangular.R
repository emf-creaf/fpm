#' Title
#'
#' @param x
#' @param y
#' @param width
#' @param normalization
#'
#' @returns
#' @export
#'
#' @examples
convolve_rectangular <- function(x, y, width = width, normalization = TRUE) {


  # Determine lower and upper limits in x.
  xmin <- min(x)
  xmax <- max(x)
  ny <- length(y)


  # A uniform distribution at each location.
  halfwidth <- width / 2
  ylow <- y - halfwidth
  yhigh <- y + halfwidth


  #
  z <- 0
  for (i in 1:ny) z <- z + dunif(x, min = ylow[i], max = yhigh[i])


  # Keep the total number of points?
  if (normalization) {
    denom <- punif(outer(xmax, y, "-")/width) - punif(outer(xmin, y, "-")/width)
    z <- sweep(z, 1, denom, FUN = "/")
  }







}
