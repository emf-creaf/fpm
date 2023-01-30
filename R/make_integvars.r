#' Sequence of
#'
#' @param species character vector containing the name of species to be used.
#' @param min_dbh numeric vector with minimum dbh. Same length as \code{species}.
#' @param max_dbh numeric vector with maximum dbh. Same length as \code{species}.
#' @param length
#'
#' @return
#' \code{data.frame} with as many named columns as the length of \code{species},
#' and with \code{length} rows. Each column corresponds to a species and it contains
#' an equispaced sequence of abscissas between \code{min_dbh} and \code{max_dbh}.
#' @export
#'
#' @examples
make_integvars <- function(species, min_dbh, max_dbh, length = length) {

  # Checks.
  if (!is.null(min_dbh)) {
    stopifnot(is.vector(min_dbh))
    attr(a, "min_dbh") <- min_dbh
  }

  if (!is.null(max_dbh)) {
    stopifnot(is.vector(max_dbh))
    attr(a, "max_dbh") <- max_dbh
  }

  if (length(species) != length(min_dbh) | length(species) != length(max_dbh))
    stop("Length of 'species', 'min_dbh' and 'max_dbh' do not match")

  if (any(is.na(species %in% names(min_dbh))) | any(is.na(species %in% names(max_dbh))))
    stop("species names in input vectors do not match")

  # Make matrix.
  x <- sapply(species, function(i) seq(min_dbh[i], max_dbh[i], length = length))

  return(x)

}
