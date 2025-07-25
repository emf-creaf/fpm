#' Set parameters of a \code{sf} \code{stand} object
#'
#' @description
#' Set parameters in a \code{sf} object that will be required during
#' calculations of stand properties
#'
#' @param sf \code{sf} stand object created with \code{\link{start_stand}}.
#' @param param named \code{list} of parameters (see \code{Details} \code{\link{start_stand}}).
#' @param verbose logical, if set to TRUE warning messages will be shown, if required.
#'
#' @return
#' A \code{sf} object with attributes \emph{country} and \emph{x} set.
#'
#' @details
#' The elements in \code{param} input list are described in \code{\link{start_stand}}.
#' Notice that parameter \code{country} must be set previously in \code{\link{start_stand}}
#' and is ignored by \code{set_parameters} with a warning.
#'
#' Setting parameter \code{integvars} automatically creates a new parameter called \code{h}
#' (also a list) which is the integration step per species.
#'
#' @export
#'
#' @examples
#' sf <- start_stands()
#' maxdbh <- list('Pinus halepensis' = 200, 'Pinus nigra' = 230)
#' sf <- set_parameters(sf, param = list(maxdbh = maxdbh, crs =  "EPSG:4326"))
#'
set_parameters <- function(sf, param = list(), verbose = T) {

  # Must be an "sf" object.
  stopifnot("Input 'sf' must be an 'sf' object" = inherits(sf, "sf"))


  # First checks.
  if (verbose) {

    # Any parameter at all?
    if (length(param) == 0) {
      warning("Input list 'param' is empty. Returning 'sf'")
      return(sf)
    }

    # Parameter "country" is ignored with a warning.
    if (!is.null(param[["country"]])) {
      warning("Parameter 'country' in input list is ignored")
      return(sf)
    }

  }


  # Check that 'country' is correct.
  attr(sf, "country") <- match.arg(param[["country"]], choices = c("spain", "usa", "france"))


  # Other parameters.
  integvars <- param[["integvars"]]
  mindbh <- param[["mindbh"]]
  maxdbh <- param[["maxdbh"]]
  crs <- param[["crs"]]

  if (!is.null(integvars)) {
    stopifnot("Parameter 'integvars' must be a named list" = is.list(integvars))
    stopifnot("Names of list elements (i.e. species) in 'integvars' are missing" = !is.null(names(integvars)))
    attr(sf, "integvars") <- integvars
    attr(sf, "h") <- sapply(integvars, function(x) x[2]-x[1], simplify = F)
  }


  if (!is.null(mindbh)) {
    stopifnot("Input 'mindbh' must be a named vector" = is.vector(mindbh))
    stopifnot("Names of elements (i.e. species) in 'mindbh' are missing" = !is.null(names(mindbh)))
    attr(sf, "mindbh") <- mindbh
  }


  if (!is.null(maxdbh)) {
    stopifnot("Input 'maxdbh' must be a named vector" = is.vector(maxdbh))
    stopifnot("Names of list elements (i.e. species) in 'maxdbh' are missing" = !is.null(names(maxdbh)))
    attr(sf, "maxdbh") <- maxdbh
  }


  if (!is.null(crs)) {
    sf::st_crs(sf) <- crs
  }


  return(sf)
}
