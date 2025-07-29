#' Fast kernel density estimation for a set of points
#'
#' @description
#' It returns the binned kernel density estimate of a discrete set of points.
#'
#' @param x vector of abscissa values where density will be evaluated.
#' @param y vector with a set of points. Length of x and y need not be the same.
#' @param type string that determines which smoothing kernel to use.
#' At the moment only type = "gaussian" or type = "uniform" have been implemented.
#' @param width If type = "gaussian", width is the standard deviation of the
#' gaussian kernel. if type = "uniform", width is the width of the bounded interval.
#' @param normalization logical, if set the output will be normalized such that
#' the area below the density estimation will be equal to the number of \code{y}
#' points.
#'
#' @details
#' Density estimation takes place without the use of FFT.
#'
#' @return vector with the binned kernel density estimate of points \code{y}.
#'
#' @examples
#'
#' # Quick implementation of trapezoidal rule.
#' trap <- function(y, h) (sum(y) - (y[1]+y[length(y)])/2)*h
#'
#' # Define points to be smoothed.
#' h <- .01
#' x <- seq(0, 100, by = h)
#' y <- runif(10)
#'
#' # With and without normalization. The output should be 10, i.e. the number
#' # of points.
#' znorm <- kernsmooth(x, y, width = 1)
#' z <- kernsmooth(x, y, width = 1, normalization = F)
#' print(c(With = trap(znorm, h), Without = trap(z, h)))
#'
#' # Wider gaussian.
#' znorm <- kernsmooth(x, y, width = 10)
#' z <- kernsmooth(x, y, width = 10, normalization = F)
#' print(c(With = trap(znorm, h), Without = trap(z, h)))
#'
#' plot(x, znorm, type = "l")
#' points(x, z, type = "l", lty = 2)
#'
#' # Example with points more spread out.
#' y <- y*50 + 20
#' znorm <- kernsmooth(x, y, width = 2)
#' plot(x, znorm, type = "l")
#' points(y,rep(0, length(y)), pch = 16, cex = 1.5)
#'
#' @export

kernsmooth <- function(x, y, type = "gaussian", width = NULL, normalization = TRUE) {


    # Checks.
  if (!is.vector(x) | !is.vector(y)) stop("Inputs 'x' and 'y' must be vectors")
  type <- match.arg(type, c("gaussian", "uniform"))
  if (is.null(width)) stop("Please supply width")
  if (length(width) > 1) stop("Width must be a single number")
  if (width <= 0) stop("width must be >0")


  # Calculating.
  z <- switch(type,
              gaussian = convolve_gaussian(x, y, width = width, normalization = normalization),
              uniform = convolve_uniform(x, y, width = width, normalization = normalization))

  return(z)
}
