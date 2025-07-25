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
#' @param by numeric, increment of the sequence.
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
#' @export
#'
#' @examples
#' mindbh <- c('Pinus nigra' = 7.5, 'Pinus pinea' = 7.5)
#' maxdbh <- c('Pinus nigra' = 200, 'Pinus pinea' = 220)
#' x <- integvars(mindbh, maxdbh, by = 10)
#'
#' # In case a given length of exactly 100 is required.
#' length <- 100
#' by <- (maxdbh-mindbh)/(length-1)
#' x <- c(drop(integvars(mindbh[1], maxdbh[1], by[1])), drop(integvars(mindbh[2], maxdbh[2], by[2])))
#'
integvars <- function(mindbh, maxdbh, by = 1) {


  # Checks.
  if (!is.vector(mindbh) | !is.vector(maxdbh)) stop("Input 'mindbh' and 'maxdbh' must be vectors")
  if (is.null(names(mindbh)) | is.null(names(maxdbh))) stop("Inputs 'mindbh' and 'maxdbh' must be named vectors")
  if (any(is.na(names(mindbh))) | any(is.na(names(maxdbh)))) stop("There are NA in names of vector elements")
  if (!identical(sort(names(mindbh)), sort(names(maxdbh)))) stop("Names in 'mindbh' and 'maxdbh' vectors do not match")


  # Compute abscissas.
  x <- sapply(names(maxdbh), function(y) seq(mindbh[y], maxdbh[y], by),
              USE.NAMES = T, simplify = F)


  return(x)

}
