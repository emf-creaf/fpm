#' Fast kernel density estimation for a set of points
#'
#' @description
#' It returns the binned kernel density estimate of a discrete set of points.
#'
#' @param x vector of abscissae values where density will be evaluated.
#' @param y vector with a set of points. Length of x and y need not be the same.
#' @param type string that determines which smoothing kernel to use.
#' At the moment only type = "gaussian" has been implemented.
#' @param width If type = "gaussian", width is the standard deviation of the
#' gaussian kernel.
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

kernsmooth <- function(x, y, type = "gaussian", width = NULL, normalization = T) {


  if (!is.vector(x) | !is.vector(y)) stop("Inputs 'x' and 'y' must be vectors")


  if (is.null(width)) stop("Please supply width")
  if (length(width) > 1) stop("Width must be a single number")
  if (width <= 0) stop("width must be >0")


  xmin <- min(x)
  xmax <- max(x)
  if (type == "gaussian") {
    w <- width^2
    w2 <- 2*w
    z <- exp(-outer(y, x, "-")^2/w2)
    if (normalization) {
      denom <- pnorm(outer(xmax, y, "-")/width) - pnorm(outer(xmin, y, "-")/width)
      z <- sweep(z, 1, denom, FUN = "/")
    }
    z <- colSums(z) / (sqrt(2*pi)*width)
  } else {
    stop("type not valid")
  }

  return(z)
}
