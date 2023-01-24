#' Composite extended Simpson's quadrature rules
#'
#' It implements two composite extended Simpson's quadrature rules, as described in
#' "Numerical Recipes" (1989) and Wolfram' web page, to estimate the
#' integral of a tabulated function.
#' Integration is 1D, although it can applied to a vector (when input is a vector)
#' or along the first dimension of an input array (when input is a 2D matrix).
#' The latter case is faster than integrating row by row.
#'
#' @param y a numeric vector or matrix that tabulates the values of a function.
#' If \code{y} is matrix, the quadrature will run along rows.
#' @param h numeric, constant interval width.
#' @param type numeric, it selects algorithm 1 (\code{type=1}) from Numerical
#' Recipes, Eq. 4.1.14, or algorithm 2 (\code{type=2}) from
#' https://mathworld.wolfram.com/Newton-CotesFormulas.html.
#'
#' @details See references.
#'
#' @return Estimated value of integral
#'
#' @examples
#'
#' ## An example function.
#' x <- seq(2, 20, length = 9)
#' y <- 1-cos(x/10*pi)
#' quad_ext_simpson(y, x[2]-x[1], 1)
#' quad_ext_simpson(y, x[2]-x[1], 2)
#'
#' ## Compare with exact value.
#' exact <- (20-sin(20/10*pi)*10/pi)-(2-sin(2/10*pi)*10/pi)
#'
#' @references
#' Press, W. H., Teukolsky, S. A., Vetterling, W. T., & Flannery, B. P. (2007).
#' Numerical recipes 3rd edition: The art of scientific computing.
#' Cambridge university press.
#'
#' @export

quad_ext_simpson <- function(y, h, type = 1) {

  if (!is.vector(y) & !is.matrix(y)) stop("y must be a vector or a matrix")
  if (!any(type %in% c(1, 2))) stop("Input 'type' must be equal to 1 or 2")

  ny <- ifelse(is.vector(y),length(y),nrow(y))

  if (type == 1) {
    if (ny < 6) stop("y must have length (if it is a vector) or number of rows (if it is a matrix) equal to or larger than 6")
    if (is.vector(y)) {
      q <- sum(y) - 5/8*(y[1]+y[ny]) + 1/6*(y[2]+y[ny-1]) - 1/24*(y[3]+y[ny-2])
    } else {
      q <- colSums(y) - 5/8*(y[1,]+y[ny,]) + 1/6*(y[2,]+y[ny-1,]) - 1/24*(y[3,]+y[ny-2,])
    }
  } else {
    if (ny < 8) stop("y must have length (if it is a vector) or number of rows (if it is a matrix) equal to or larger than 8")
    if (is.vector(y)) {
      q <- sum(y) - 31/48*(y[1]+y[ny]) + 11/48*(y[2]+y[ny-1]) - 5/48*(y[3]+y[ny-2]) + 1/48*(y[4]+y[ny-3])
    } else {
      q <- colSums(y) - 31/48*(y[1,]+y[ny,]) + 11/48*(y[2,]+y[ny-1,]) - 5/48*(y[3,]+y[ny-2,]) + 1/48*(y[4,]+y[ny-3,])
    }
  }

  return(h*q)

}
