#' Density of truncated exponential distribution
#'
#' Density for the truncated exponential distribution, given its rate and the lower and/or lower truncation limits.
#'
#' @param x vector of quantiles.
#' @param rate rate of the exponential distribution.
#' @param min value of the lower truncation limit. Default is 0.
#' @param max value of the upper truncation limit. Default is infinite.
#'
#' @details
#' Simple calculation of the density of the truncated exponential distribution.
#'
#' @return density of the truncated exponential distribution.
#'
#' @examples
#'
#' ## A truncated exponential distribution.
#' rate <- .1
#' x <- seq(3,8,by=.01)
#' y <- dtrexp(x,rate=rate,min=3,max=8)
#' plot(x,y,type="l")
#'
#' # Its integral must be =1 for y to be a distribution.
#' trap <- function(y, h) (sum(y) - (y[1]+y[length(y)])/2)*h
#' trap(y,x[2]-x[1])
#'
#' @export

dtrexp <- function(x, rate = 1, min = 0, max = NULL) {

  if (rate<=0) stop("rate must be > 0")
  if (!is.null(max)) if (max <= min) stop("max must be > min")
  y <- if (is.null(max)) {
    rate*exp(-rate*x)/exp(-rate*min)
  } else {
    if (min(x)<min | max(x)>max) stop("x should be min <= x <= max")
    rate*exp(-rate*x)/(exp(-rate*min)-exp(-rate*max))
  }
  return(y)
}
