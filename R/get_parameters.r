#' Extract parameters from \code{stand} object
#'
#' @description
#' Extraction of global parameters from \code{stand} object
#'
#'
#' @param a \code{sf} object containing a number of POINT geometry types
#' @param parameters character vector with names of parameters to be extracted:
#' \code{country}, \code{integvars}, \code{h}, \code{min_dbh}, \code{max_dbh} or \code{crs}.
#'
#' @return
#'
#' If only parameters is provided
#'
#' @export
#'
#' @details
#' Additional details...
#'
#'
#' @examples
get_parameters <- function(a, parameters = NULL) {

  # Must be an "sf" object.
  stopifnot("Input 'a' must be an 'sf' object" = any(class(a) == "sf"))

  if (is.null(parameters)) parameters <- c("country", "integvars", "h", "min_dbh", "max_dbh", "crs")
  b <- sapply(parameters, function(x) attr(a, match.arg(x, c("country", "integvars", "h", "min_dbh", "max_dbh", "crs"))), simplify = F)
  if (length(parameters) == 1) b <- b[[1]]

  return(b)
}
