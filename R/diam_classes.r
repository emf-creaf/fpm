#' Quadrature of a tabulated function at discrete intervals
#'
#' @description
#' \code{diam_classes} returns the integral of a set of points at different intervals.
#'
#' @param x vector of abscissa values. It should be strictly increasing and have
#' the same length as \code{y}
#' @param y vector of ordinate values, corresponding to every value in \code{x}.
#' @param xl vector with limits of class intervals. Its range should be included
#' in the range of \code{x}.
#'
#' @return
#' A vector containing the value of the numerical quadrature of tabulated function
#' \code{y} within intervals defined by \code{xl}.
#'
#' @details
#' Tabulated function (\code{x, y}) is integrated within the integration limits
#' given by \code{xl}. If there is an interval in \code{xl} that does not include
#' any value of \code{x}, the corresponding elements of the output vector will
#' contain a NA.
#'
#' @export
#'
#' @examples
#'
#' # Example sinus curve.
#' x <- seq(-3, 60, by=.17)
#' y <- sin(x*pi/54)
#'
#' # The exact value of this integral between 0 and 54 is (1-cos(pi))*54/pi=34.37747
#' xl <- seq(0, 54, length = 10)
#' q1 <- diam_classes(x, y, xl)
#' q2 <- diam_classes(x, y, xl, F)
#' plot((xl[-1]+xl[-length(xl)])/2, q1,xlab="Num. intervals",ylab="Integral value",pch=1)
#' points((xl[-1]+xl[-length(xl)])/2, q2, pch=2)
#' legend("topleft", pch=c(1,2), legend = c("Corrected","Uncorrected"))
#'
#' # For a constant number of abscissas, the precision of the approximation degrades
#' as the number of intervals increases.
#' xl <- c(0,54)
#' q1 <- sum(diam_classes(x, y, xl), na.rm = T)
#' q2 <- sum(diam_classes(x, y, xl, F), na.rm = T)
#' for (i in 1:100) {
#'   xl <- c(0,sort(runif(i)),1)*54
#'   q1 <- c(q1, sum(diam_classes(x, y, xl), na.rm = T))
#'   q2 <- c(q2, sum(diam_classes(x, y, xl, F), na.rm = T))
#' }
#' plot(1:101, q1,ylim=c(25,35),xlab="Num. intervals",ylab="Integral value",pch=1)
#' points(1:101, q2, pch=2)
#' z <- (1-cos(pi))*54/pi
#' points(c(1,101), c(z,z), type = "l", lwd = 2, lty = 2)
#' legend("bottomleft", pch=c(1,2,NA), lty=c(NA,NA,2),
#' legend = c("Corrected","Uncorrected","Exact"))
#'

diam_classes <- function(x, y, xl, correction = T) {

  # Are all inputs provided?
  mf <- match.call()
  m <- match(c("x", "y", "xl"), tolower(names(mf)[-1]))
  if (any(is.na(m))) stop("Missing inputs")

  # Are they vectors?
  if (!all(is.vector(x) & is.vector(y) & is.vector(xl))) stop("All three inputs should be vectors")

  # Other checks.
  nx <- length(x)
  nxl <- length(xl)
  if (nx <= nxl) stop("Length of 'x' should not be smaller than that of 'xl'")
  if (nxl < 2) stop("Vector 'xl' should have at least two points")
  if (nx != length(y)) stop("Length of 'x' and 'y' should match")
  if (min(xl) < min(x) | (max(xl) > max(x))) stop("Range of 'xl' cannot be outside range of 'x'")
  if (is.unsorted(x, strictly = T)) stop("Vector 'x' should have strictly increasing values")
  if (var(diff(x)) > .Machine$double.eps) stop("Increments in 'x' should be constant")
  if (is.unsorted(xl, strictly = T)) stop("Vector 'xl' should be monotonically increasing")
  if (!is.logical(correction)) stop("Input 'correction' must be logical")

  # If interval does not match exactly points in x, we implement a very simple procedure.
  # In short, at each interval in xi the quadrature is multiplied by the proportion of
  # the interval that is not within the limits of the integration.
  h <- x[2]-x[1]
  dxl <- diff(xl)
  j <- findInterval(x, xl)
  q <- numeric(nxl-1)
  for (i in 1:(nxl-1)) {
    k <- which(j == i)

    lk <- length(k)

    # No points within the interval.
    if (lk == 0) {
      q[i] <- NA
    } else {

      # If there is only one point within the interval, apply midpoint quadrature.
      if (lk == 1) {
        q[i] <- y[k] * ifelse(correction, dxl[i], h)
      } else {

        # If there is more than one point, apply trapezoidal quadrature.
        q[i] <- MiscMath::quad_trapez(y[k], h)
        if (correction) {
          xx <- x[k]
          q[i] <- q[i]*dxl[i]/(max(xx)-min(xx))
        }
      }
    }
  }

  return(q)
}
