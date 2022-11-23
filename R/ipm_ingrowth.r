#' Compute tree ingrowth
#'
#' @details
#' \code{ipm_ingrowth} computes the number and size distribution of new trees
#'
#' @param a a \code{sf} object containing a number of POINT geometry types.
#' @param idplot identifiers of POINT elements representing tree stands to remove.
#' @param num_ingr named vector containing the number of new trees per species.
#' @param lambda named vector with the rate parameter of the truncated
#' exponential distribution per species.
#' @param min_dbh named vector containing the cutoff value for the dbh above
#' which an individual plant is considered an adult tree.
#' @param scaled_ha logical indicating whether the number of ingrowth trees
#' must be scaled up to hectare.
#'
#' @return
#' @export
#'
#' @examples
ipm_ingrowth <- function(a, idplot, num_ingr, lambda, min_dbh, scaled_ha = T) {

  # Check idplot.
  id <- match(idplot, a$idplot)
  if (is.na(id)) stop("Could not find 'idplot' in 'a'")
  if (length(id) != 1) stop("Only one 'idplot' can be modified at the time")
  if (a$stand_type[id] != "ipm") stop("'stand_type' must be 'ipm'")

  # Species.
  sp <- names(num_ingr)
  if (length(sp) != length(lambda)) stop("Length of 'num_sapl' and 'lambda' are not equal")
  if (!all(sp %in% names(lambda)))
    stop("Inputs 'num_sapl' and 'lambda' have different species")

  # Check min_dbh.
  if (!all(sp %in% names(min_dbh))) stop("Species and integvars column names do not match")

  # Abscissas per species.
  x <- attr(a, "integvars")
  if (!all(sp %in% colnames(x))) stop("Species and integvars column names do not match")

  # Select tree data.frame.
  b <- a[id, ]$trees[[1]]

  # Species loop.
  country <- tolower(attr(a, "country"))
  for (i in sp) {

    if (country == "spain") {
      # Truncated exponential distribution times number of saplings.
      q <- MiscStat::dtrexp(x[, i], lambda[i], min = min_dbh[i]) * num_ingr[i]

      # New ingrowth trees are added to the tree distribution. If the species
      # is not already present as tree, a new column is added to the tree stand.
      if (i %in% colnames(b)) {
        b[, i] <- b[, i] + q
      } else {
        b[paste0(i)] <- q
      }
    } else if (country == "france") {
    } else if (country == "usa") {
    }
  }

  # Up to ha?
  if (scaled_ha) {
    switch(country,
           spain = b <- b * (10000/pi/5)
    )
  }

  # New trees.
  a[id, ]$trees[[1]] <- b

  return(a)
}
