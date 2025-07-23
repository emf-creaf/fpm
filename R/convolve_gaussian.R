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
#'
#' # Quick implementation of trapezoidal rule.
#' trap <- function(y, h) (sum(y) - (y[1]+y[length(y)])/2)*h
#'
#' # Define points to be smoothed.
#' h <- 0.1
#' x <- seq(7.5, 100, by = h)
#' y <- runif(10)*10 + 7.5
#' z <- convolve_gaussian(x, y, width = 5, normalization = FALSE)
#' znorm <- convolve_gaussian(x, y, width = 5)
#' plot(x, znorm, type = "l", xlim = c(0, 40))
#' points(x, z, type = "l", lty = 2, lwd = 2)
#' points(y, rep(0, length(y)), pch = 16, cex = 2)
#' legend("topright", legend = c("Normalized", "Not normalized"), lty = 1:2)
#'
#' # Total number of points for normalized and not normalized.
#' print(c(With = trap(znorm, h), Without = trap(z, h)))
convolve_gaussian <- function(x, y, width = width, normalization = TRUE) {


  # Checks.
  if (!is.vector(x) | !is.vector(y)) stop("Inputs 'x' and 'y' must be vectors")


  # Determine lower and upper limits in x.
  xmin <- min(x)
  xmax <- max(x)


  # Width parameters used below.
  w <- width^2
  w2 <- 2*w


  # A Gaussian at each location.
  z <- exp(-outer(y, x, "-")^2/w2)


  # Keep the total number of points?
  if (normalization) {
    denom <- pnorm(outer(xmax, y, "-")/width) - pnorm(outer(xmin, y, "-")/width)
    z <- sweep(z, 1, denom, FUN = "/")
  }


  # Final denominator factor for Gaussian.
  z <- colSums(z) / (sqrt(2*pi)*width)


  return(z)
}
