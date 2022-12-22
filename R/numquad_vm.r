#' Quadrature of product of vector and matrix
#'
#' @details
#' \code{numquad_vm} computes the numerical quadrature of the product between
#' a vector \code{v} and a matrix \code{m}.
#'
#' @param v numeric vector.
#' @param m numeric matrix.
#' @param nv length of \code{v}. It should match the length of vector \code{v}
#' and the number of rows of matrix \code{m}.
#' @param h single number specifying sub-interval size.
#' @param quadrature character specifying whether to use the trapezoidal rule
#' (\code{quadrature} = "trapezoidal") or the extended Simpson rule
#' (\code{quadrature} = "simpson") all in lower case letters.
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
#' @export
#'
#' @examples
#' # Use lower numbers for nv and nc if computing power/RAM is an issue for you.
#' nv <- 5000
#' nc <- 100
#' v <- seq(1, 10, length = nv)
#' m <- matrix(runif(nv*nc), nv, nc)
#' h <- .1
#' x <- numquad_vm(v, m, nv, h)
#' y <- sapply(1:nc, function(i) MiscMath::quad_trapez(v*m[,i], h))
#' print(mad(x-y))
#'
#' # Just how much faster than quad_trapez is it?
#' niter <- 1000
#' t1 <- system.time(replicate(niter, x <- numquad_vm(v, m, nv, h)))
#' t2 <- system.time(replicate(niter, y <- sapply(1:nc, function(i) MiscMath::quad_trapez(v*m[,i], h))))
#'
#' print(t2/t1)
#' print(sum(t2,na.rm=T)/sum(t1,na.rm=T))
#'
numquad_vm <- function(v, m, nv, h, quadrature = c("trapez", "simpson")) {

  # Checks.
  if (!is.vector(v)) stop("Input 'v' must be a vector")
  if (!is.matrix(m)) stop("Input 'm' must be a matrix")
  if ((length(nv) != 1) | length(h) != 1) stop("'nv' and 'h' must be single numbers")
  if (length(v) != nv) stop("Length of vector 'v' must be equal to nv")
  if (nv != nrow(m)) stop("Length of 'v' must be equal to number of rows of 'm'")

  quadrature <- match.arg(quadrature)

  # Matrix product.
  y <- v %*% m

  # Correction.
  if (quadrature == "trapez") {
    y <- y - .5*(v[1]*m[1, ] + v[nv]*m[nv, ])
  } else if (quadrature == "simpson") {
    y <- y - 7/12*(v[1]*m[1, ] + v[nv]*m[nv, ]) + 1/12*(v[2]*m[2, ] + v[nv-1]*m[nv-1, ])
  }

  return(drop(y)*h)

}
