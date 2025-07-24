#' Convolve a discrete series of points with an uniform distribution.
#'
#' @description
#' \code{convolve_uniform} centers an uniform distribution at the location of input points,
#' effectively convoluting the point series with an box-car window.
#'
#' @param x \code{numeric} vector with abscissa values.
#' @param y \code{numeric} vector containing the points at which to center the uniform distributions.
#' @param width \code{numeric} with the width of the interval of the uniform distribution.
#' If it has more than one element, one the first one is used.
#' @param normalization \code{logical}, if set to TRUE the total area of the resulting vector
#' is equal to the number of elements in \code{y}.
#'
#' @returns
#' A \code{numeric} vector of the same length as that of \code{x} where each location
#' \code{y} has been substituted by a rectangle of width \code{width} and area equal to 1.
#' Rectangles may overlap, in which case they all add up.
#'
#' @details
#' At each \code{y} location a centered uniform distribution of width \code{width} is calculated.
#' When the tails of several curves overlap, the results add those curves up.
#'
#' @export
#'
#' @examples
#' # Quick implementation of midpoint rule. Better than trapezoidal for this example.
#' trap <- function(y, h) (sum(y) - (y[1]+y[length(y)])/2)*h
#'
#' # Define points to be smoothed. Reduce h for better approximation.
#' h <- 0.1
#' x <- seq(7.5, 50, by = h)
#' y <- c(7, 8, 8.5, 9, 29, 49, 49.5, 50)
#' z <- convolve_uniform(x, y, width = 5, normalization = FALSE)
#' znorm <- convolve_uniform(x, y, width = 5)
#' plot(x, znorm, type = "l", xlim = c(0, 50))
#' points(x, z, type = "l", lty = 2, lwd = 2)
#' points(y, rep(0, length(y)), pch = 16, cex = 2)
#' legend("topright", legend = c("Normalized", "Not normalized"), lty = 1:2)
#'
#' # Total number of points for normalized and not normalized.
#' print(c(With = trap(znorm, h), Without = trap(z, h), Expected = 8))
#'
convolve_uniform <- function(x, y, width = width, normalization = TRUE) {


  # Checks.
  if (!is.vector(x) | !is.vector(y)) stop("Inputs 'x' and 'y' must be vectors")
  if (length(width) > 1) width <- width[1]


  # Parameters needed below.
  ylow <- y - width / 2
  yhigh <- y + width / 2


  # Uniform distribution centered at each point.
  ny <- length(y)
  z <- sapply(1:ny, function(i) dunif(x, min = ylow[i], max = yhigh[i]))


  # Keep the total number of points?
  if (normalization) {

    # Determine lower and upper limits for denominator.
    ymin <- pmax(min(x), ylow)
    ymax <- pmin(max(x), yhigh)

    # Calculate the normalization factor and apply it.
    denom <- sapply(1:ny, function(i) {
      punif(ymax[i], min = ylow[i], max = yhigh[i]) - punif(ymin[i], min = ylow[i], max = yhigh[i])
    })
    z <- sweep(z, MARGIN = 2, STATS = denom, FUN = "/")
  }


  return(apply(z, 1, sum))
}
