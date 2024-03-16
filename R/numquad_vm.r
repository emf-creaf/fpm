#' Quadrature of product of vector and matrix
#'
#' @details
#' \code{numquad_vm} computes the numerical quadrature of the product between
#' a vector \code{v} and a matrix \code{m}.
#'
#' @param v numeric vector.
#' @param m numeric matrix.
#' @param h single number specifying sub-interval size.
#' @param method character specifying whether to use the trapezoidal rule
#' (\code{method} = "trapezoidal") or the extended Simpson rule
#' (\code{method} = "simpson") all in lower case letters.
#'
#' @return
#' numeric vector with the numerical estimation of the quadrature.
#'
#' @details
#' \code{numquad_vm} first uses matrix multiplication between vector \code{v}
#' and matrix \code{m} for a fast estimation of the integral. Then, a correction
#' for the two extremes of the integration interval is performed depending on
#' the type of numerical quadrature that has been selected.
#'
#' @references
#' See Eq.(36) in https://mathworld.wolfram.com/Newton-CotesFormulas.html
#'
#' @export
#'
#' @examples
#' # Use lower numbers for nv and nc if computing power/RAM is an issue for you.
#' nv <- 5000
#' nc <- 100
#' v <- seq(1, 10, length = nv)
#' m <- matrix(runif(nv*nc), nv, nc)
#' h <- .1
#' x <- numquad_vm(v, m, h)
#'
numquad_vm <- function(v, m, h, method = "trapez") {

  # Check inputs.
  stopifnot("Input 'v' should be a vector" = is.vector(v))
  stopifnot("Input 'm' should be a matrix" = is.matrix(m))
  stopifnot("Input 'h' should be a single numeric numb" = h > 0)
  nv <- length(v)
  stopifnot("Length of 'v' must be equal to number of rows of 'm'" = nv == nrow(m))


  # Check quadrature.
  quadrature <- match.arg(tolower(method), c("trapez", "simpson"))


  # Matrix product.
  y <- v %*% m


  # Taking care of left and right extremes.
  if (method == "trapez") {
    y <- y - .5*(v[1]*m[1, ] + v[nv]*m[nv, ])
  } else {
    y <- y - 7/12*(v[1]*m[1, ] + v[nv]*m[nv, ]) + 1/12*(v[2]*m[2, ] + v[nv-1]*m[nv-1, ])
  }

  return(drop(y)*h)

}
