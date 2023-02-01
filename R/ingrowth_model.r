#' Title
#'
#' @param obj regression object with which to predict.
#' @param newdata one-row numeric \code{data.frame} containing the values of the
#' predictor variables.
#' @param x numeric vector containing the abscissas at which to calculate the
#' distribution of new trees.
#' @param lambda numeric, rate parameter for truncated exponential distribution.
#' @param min_dbh numeric, minimum abscissa value.
#' @param scale_ha logical, if set the results will be scaled up to ha.
#'
#' @return
#' @export
#'
#' @examples
ingrowth_model <- function(obj, newdata, x, lambda, min_dbh, scale_ha = T) {

  # Number of new trees.
  N <- predict(obj, newdata = newdata, type = "response")

  # Size distribution of new trees is a truncated exponential.
  y <- dtrexp(x, rate = lambda, min = min_dbh)

  # Scale up to ha from the small 5-m plot.
  if (scale_ha) N <- N * (10000/(pi*5^2))

  return(N*y)
}
