#' Check the existence of plot
#'
#' @description
#' \code{check_exist} checks the existence of plots within a \code{data.frame},
#' \code{tibble} or \code{sf} objects by matching the \code{idplot} field.
#'
#' @param a a \code{sf} object containing a number of POINT geometry types.
#' @param idplot identifier of single POINT, representing a tree stand, to modify.
#'
#' @return \code{check_exist} returns a named logical vector of the same length as
#' \code{idplot} with TRUE or FALSE depending on whether or not \code{idplot} values
#' match those in the \code{idplot} value of \code{a}.
#'
#' @export
#'
#' @examples
#' a <- start_stand(paste0("ID", 1:10), runif(10), runif(10), "EPSG:4326")
#' check_exist(a, paste0("ID", 3:25))
check_exist <- function(a, idplot) {
  i <- match(idplot, a$idplot)
  return(setNames(idplot, ifelse(is.na(i), FALSE, TRUE)))
}
