#' Trapezoidal quadrature rule
#'
#' It implements the trapezoidal quadrature rule.
#' Integration is 1D, although it can applied to a vector (when input is a vector) or along the first dimension
#' of an input array (when input is a matrix). The latter case is faster than integrating row by row.
#'
#' @param y a vector of ordinate values. It should have 5 or more points.
#' @param h interval width.
#'
#' @details
#'
#' @return estimated value of integral
#'
#' @examples
#'
#' ## An example function.
#' x <- 2:20
#' y <- 1-cos(x/10*pi)
#' quad_trapez(y,1)
#'
#' ## Compare with exact value.#'
#' (20-sin(20/10*pi)*10/pi)-(2-sin(2/10*pi)*10/pi)
#'
#' ## A more difficult example.
#' x <- 0:100
#' y <- x*dnorm(x,mean=0,sd=10)
#' quad_trapez(y,1)
#'
#' ## Compare with exact value.
#' 10^2*(dnorm(min(x),mean=0,sd=10)-dnorm(max(x),mean=0,sd=10))
#'
#' ## Integration along first dimension of a matrix with h=0.1
#' y <- matrix(runif(2000),50,40)
#' qy <- quad_trapez(y,0.1)
#'
#' @export

quad_trapez <- function(y, h) {

  if (!is.vector(y) & !is.matrix(y)) stop("y must be a vector or a matrix")

  ny <- ifelse(is.vector(y),length(y),nrow(y))

  if (is.vector(y)) {
    q <- sum(y) - 0.5*(y[1]+y[ny])
  } else {
    q <- colSums(y) - 0.5*(y[1,]+y[ny,])
  }

  return(h*q)

}
