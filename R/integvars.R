#' Abscissas for the numerical quadrature
#'
#' @description
#' \code{integvars} calculates the dbh abscissas per species that are used
#' during the numerical quadrature of the model.
#'
#' @param mindbh named numeric vector containing the minimum dbh for adults trees
#' per species.
#' @param maxdbh named numeric vector containing the maximum dbh for adults trees
#' per species. Names of \code{mindbh} and \code{maxdbh} must match exactly.
#' @param by numeric, increment of the sequence. If \code{by=1} then all abscissas will
#' have the same increment of 1. If length of \code{by} is larger than 1, then it must be
#' a named vector and its length and names must match those of \code{mindbh} and \code{maxdbh}.
#'
#' @return
#' A list, with elements named as the species in \code{mindbh} and \code{maxdbh},
#' and where each element consists of a vector of ascissas running from
#' the corresponding minimum to maximum dbh values.
#'
#' @details
#' Simple implementation of the \code{seq} function. The option \code{length},
#' however, is not available. See examples below.
#'
#' When length of vector \code{by} matches that of \code{mindbh} and \code{maxdbh},
#' each species will have abscissas with different increments.
#'
#' @export
#'
#' @examples
#'
#' mindbh <- c('Pinus nigra' = 7.5, 'Pinus pinea' = 7.5)
#' maxdbh <- c('Pinus nigra' = 200, 'Pinus pinea' = 220)
#'
#' # Same increment for all.
#' x <- integvars(mindbh, maxdbh, by = 10)
#' print(sapply(x, length))
#' print(sapply(sapply(x, diff), mean))
#'
#' # Different increment per species.
#' x <- integvars(mindbh, maxdbh, by = c('Pinus nigra' = .5, 'Pinus pinea' = .22))
#' print(sapply(x, length))
#' print(sapply(sapply(x, diff), mean))
#'
#' # In case a given length of exactly 100 is required.
#' length <- 100
#' by <- (maxdbh-mindbh)/(length-1)
#' x <- integvars(mindbh, maxdbh, by)
#' print(sapply(x, length))
#' print(apply(sapply(x, diff), 2, mean))
#'
integvars <- function(mindbh, maxdbh, by = 1) {


  # Checks.
  if (!is.vector(mindbh) | !is.vector(maxdbh)) stop("Input 'mindbh' and 'maxdbh' must be vectors")
  if (is.null(names(mindbh)) | is.null(names(maxdbh))) stop("Inputs 'mindbh' and 'maxdbh' must be named vectors")
  if (any(is.na(names(mindbh))) | any(is.na(names(maxdbh)))) stop("There are NA in names of vector elements")
  if (length(mindbh) != length(maxdbh)) stop("Length of 'mindbh' and 'maxdbh' should be the same")
  if (!identical(sort(names(mindbh)), sort(names(maxdbh)))) stop("Names in 'mindbh' and 'maxdbh' vectors do not match")
  if (any(by <= 0)) stop("Input parameter 'by' should be strictly positive")


  # Parameter 'by' can be a vector, in which case it must have the same length as 'mindbh' and 'maxdbh'.
  if (length(by) > 1) {
    if (length(by) != length(mindbh)) stop("Length of parameter vector 'by' is wrong")
    if (!identical(sort(names(by)), sort(names(maxdbh)))) stop("Names in vector 'by' do not match that of 'mindbh' or 'maxdbh'")
    x <- sapply(names(maxdbh), function(y) seq(mindbh[y], maxdbh[y], by[y]), USE.NAMES = T, simplify = F)
  } else {
    x <- sapply(names(maxdbh), function(y) seq(mindbh[y], maxdbh[y], by), USE.NAMES = T, simplify = F)
  }


  return(x)

}
