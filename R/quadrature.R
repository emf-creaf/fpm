#' 1-D composite numerical quadrature of vector or matrix.
#'
#' @description
#' It calculates a 1-D numerical quadrature of a vector or matrix with the Composite
#' Trapezoidal or the Alternative Extended Simpson' rule.
#'
#' @param y \code{numeric} vector or matrix. If matrix, the integration will take place
#' along rows.
#' @param h \code{numeric}, step size of the quadrature. It must be a positive number.
#' @param type \code{character}, it should be equal to "trapezoidal" (default) or "simpson"
#' (case insensitive).
#'
#' @details
#' The trapezoidal rule corresponds to the classical trapezoid formula,
#' which works by approximating the function by equal-width trapezoids.
#' \code{type} equal to "simpson" corresponds to the Alternative extended Simpson's rule
#' (see https://en.wikipedia.org/wiki/Simpson%27s_rule#Composite_Simpson's_1/3_rule and
#' references therein).
#'
#' @return
#' A single number (if y is a vector) and a vector (if y is a matrix) with the value of
#' the integration.
#'
#' @export
#'
#' @examples
#' n <- 101
#' x <- seq(0, pi/4, len = n)
#' y <- cos(x)
#' h <- x[2]-x[1]
#'
#' # Actual value of the integral of \code{cos} between 0 and pi is \code{sin(pi/4)} = 0.7071068
#' q1 <- c('Trapezoidal' = quadrature(y, h), 'Simpson' = quadrature(y, h, "simpson"))
#' my <- cbind(y, y, y)
#' q2 <- data.frame('Trapezoidal' = quadrature(my, h), 'Simpson' = quadrature(my, h, "simpson"))
#'
quadrature <- function(y, h = 1, type = "trapezoidal") {


  # Check inputs.
  stopifnot("Input 'y' must be a vector or a matrix" = is.vector(y) | is.matrix(y))
  stopifnot("Input 'h' must be a single positive number" = (length(h) == 1) & (h > 0))
  type <- tolower(type)
  stopifnot("Input 'type' must be equal to 'trapezoidal' or 'simpson" = any(type == c("trapezoidal", "simpson")))


  # More checks.
  ny <- ifelse(is.vector(y), length(y), nrow(y))
  type <- match.arg(type, c("trapezoidal", "simpson"))
  if (is.vector(y)) {
    if (type == "trapezoidal") {
      stopifnot("Length of vector 'y' must be 3 or more for trapezoidal quadrature" = length(y) > 2)
    } else {
      stopifnot("Number of rows in matrix 'y' must be 3 or more for Simpson's quadrature" = nrow(y) > 2)
    }
  } else {
    if (type == "trapezoidal") {
      stopifnot("Length of vector 'y' must be 9 or more for alternative extended Simpson' quadrature" = length(y) > 2)
    } else {
      stopifnot("Number of rows in matrix 'y' must be 9 or more for alternative extended Simpson' quadrature" = nrow(y) > 2)
    }
  }


  # Quadrature for vector or matrix.
  if (is.vector(y)) {
    if (type == "trapezoidal") {
      q <- sum(y) - 0.5*(y[1]+y[ny])
    } else {
      q <- sum(y) - (31*(y[1]+y[ny]) - 11*(y[2]+y[ny-1]) + 5*(y[3]+y[ny-2]) - 1*(y[4]+y[ny-3]))/48
    }
  } else {
    if (type == "trapezoidal") {
      q <- colSums(y) - 0.5*(y[1,]+y[ny,])
    } else {
      q <- colSums(y) - (31*(y[1,]+y[ny,]) - 11*(y[2,]+y[ny-1,]) + 5*(y[3,]+y[ny-2,]) - 1*(y[4,]+y[ny-3,]))/48
    }
  }

  return(h*q)

}
