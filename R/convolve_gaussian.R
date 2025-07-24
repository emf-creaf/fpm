#' Convolve a discrete series of points with a Gaussian distribution.
#'
#' @description
#' \code{convolve_gaussian} centers a Gaussian distribution at the location of input points,
#' effectively convoluting the point series with an Gaussian window.
#'
#' @param x \code{numeric} vector with abscissa values.
#' @param y \code{numeric} vector containing the points at which to center the Gaussian distributions.
#' @param width \code{numeric} with the standard deviation of the Gaussian distribution.
#' If it has more than one element, one the first one is used.
#' @param normalization \code{logical}, if set to TRUE the total area of the resulting vector
#' is equal to the number of elements in \code{y}.
#'
#' @returns
#' A \code{numeric} vector of the same length as that of \code{x} where each location
#' \code{y} has been substituted by a Gaussian curve of standard deviation \code{width}
#' and area equal to 1. Curves ma may overlap, in which case they all add up.
#'
#' @details
#' At each \code{y} location a centered Gaussian curve with standard deviation \code{width} and
#' domain [min(x), max(x)] is calculated. When the tails of several curves overlap, the results
#' add those curves up.
#'
#' @export
#'
#' @examples
#'
#' # Quick implementation of trapezoidal rule.
#' trap <- function(y, h) (sum(y) - (y[1]+y[length(y)])/2)*h
#'
#' # Define points to be smoothed. Reduce h for better approximation.
#' h <- 0.01
#' x <- seq(7.5, 50, by = h)
#' y <- c(7, 8, 8.5, 9, 29, 49, 49.5, 50)
#' z <- convolve_gaussian(x, y, width = 5, normalization = FALSE)
#' znorm <- convolve_gaussian(x, y, width = 5)
#' plot(x, znorm, type = "l", xlim = c(0, 50))
#' points(x, z, type = "l", lty = 2, lwd = 2)
#' points(y, rep(0, length(y)), pch = 16, cex = 2)
#' legend("topright", legend = c("Normalized", "Not normalized"), lty = 1:2)
#'
#' # Total number of points for normalized and not normalized.
#' print(c(With = trap(znorm, h), Without = trap(z, h), Expected = 8))
convolve_gaussian <- function(x, y, width = width, normalization = TRUE) {


  # Checks.
  if (!is.vector(x) | !is.vector(y)) stop("Inputs 'x' and 'y' must be vectors")
  if (length(width) > 1) width <- width[1]


  # Gaussian distribution centered at each point.
  z <- exp(-outer(y, x, "-")^2/(2*width^2))


  # Keep the total number of points?
  if (normalization) {

        # Calculate the normalization factor and apply it.
    denom <- pnorm(outer(max(x), y, "-")/width) - pnorm(outer(min(x), y, "-")/width)
    z <- sweep(z, 1, denom, FUN = "/")
  }


  # Final denominator factor for Gaussian.
  z <- colSums(z) / (sqrt(2*pi)*width)


  return(z)
}
