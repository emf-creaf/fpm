#' The Log Normal Distribution (modified)
#'
#' @description
#' Density of the log-normal distribution.
#'
#' @param x vector of quantiles.
#' @param meanlog mean of the distribution on the log scale.
#' @param sdlog standard deviation of the distribution on the log scale.
#'
#' @details
#' Output is exactly the same than that of the built-in \code{stats} R function
#' \code{dlnorm}, but with the caveat that the case x = 0, meanlog = -Inf and sdlog = 0
#' now gives 0, instead of Inf as in \code{dlnorm}.
#'
#' Notice that \code{log} is not given as an option, since \code{dln} is for internal
#' use of the package and option \code{log=F} always for our purposes.
#'
#' For a full description see \code{dlnorm} help page.
#'
#' @return
#' Density of the log-normal distribution.
#'
#' @export
#'
#' @examples
#'
#' # Notice the difference.
#' dlnorm(0, -Inf, 0)
#' dln(0, -Inf, 0)
#'
dln <- function(x, meanlog = 0, sdlog = 1) {


  # The "stats" dlnorm function.
  y <- dlnorm(x, meanlog, sdlog)


  # Check the special case dlnorm(x = 0, meanlog = -Inf, sdlog = 0) = Inf.
  # For our purposes, it should give 0.
  i <- is.infinite(y)
  if (sum(y) > 0) y[i] <- 0


  return(y)
}
