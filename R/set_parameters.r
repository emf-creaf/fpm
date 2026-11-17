#' Set parameters of a \code{sf} \code{stand} object
#'
#' @description
#' Set parameters in a \code{sf} object that will be required during
#' calculations of stand properties
#'
#' @param sf_plots \code{sf} object..
#' @param param named \code{list} of parameters (see \code{Details} \code{\link{start_stand}}).
#' @param verbose logical, if set to TRUE warning messages will be shown, if required.
#'
#' @return
#' A \code{sf} object with attributes \emph{country} and \emph{x} set.
#'
#' @details
#' The elements in \code{param} input list are described in \code{\link{start_stand}}.
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
set_parameters <- function(sf_plots = NULL, param = NULL, verbose = TRUE) {


  if (verbose) {
    fn_name <- as.character(sys.call()[[1]])
    cat("\n")
  }


  # Checks.
  if (is.null(sf_plots) || !inherits(sf_plots, "sf")) {
    cli::cli_abort("Parameter 'sf_plots' must be an 'sf' object")
  }

  if (is.null(param) || !is.list(param)) {
    cli::cli_abort("Wrong input list 'param'")
  }

  param_names <- names(param)
  if (!any(param_names %in% c("integvars", "h", "mindbh", "maxdbh", "crs"))) {
    cli::cli_abort("Wrong element names in 'param' list")
  }

  # Other parameters.
  integvars <- param[["integvars"]]
  mindbh <- param[["mindbh"]]
  maxdbh <- param[["maxdbh"]]
  h <- param[["h"]]
  crs <- param[["crs"]]

  if (!is.null(integvars)) {
    if (!is.list(integvars)) {
      cli::cli_abort("Parameter 'integvars' must be a named list")
    }
    if (is.null(names(integvars))) {
      cli::cli_abort("Names of list elements (i.e. species) in 'integvars' are missing")
    }

    if (verbose) {
      cli::cli_alert_info("[{fn_name}] Setting attribute 'integvars'")
    }

    attr(sf_plots, "integvars") <- integvars

  }


  if (!is.null(h)) {
    if (!is.list(h) || is.null(names(h))) {
      cli::cli_abort("Parameter 'h' must be a named list object")
    }

    if (verbose) {
      cli::cli_alert_info("[{fn_name}] Setting attribute 'h'")
    }

    attr(sf_plots, "h") <- h
  } else {

    if (verbose) {
      cli::cli_alert_info("[{fn_name}] Calculating and setting attribute 'h' from 'integvars'")
    }

    attr(sf_plots, "h") <- sapply(integvars, function(x) x[2]-x[1], simplify = F)
  }


  if (!is.null(mindbh)) {
    if (!is.list(mindbh)) {
      cli::cli_abort("Input 'mindbh' must be a 'list' object")
    }
    if (is.null(names(mindbh))) {
      cli::cli_abort("Elements in list 'mindbh' are missing")
    }

    if (verbose) {
      cli::cli_alert_info("[{fn_name}] Setting attribute 'mindbh'")
    }

    attr(sf_plots, "mindbh") <- mindbh
  }


  if (!is.null(maxdbh)) {
    if (!is.list(mindbh)) {
      cli::cli_abort("Input 'maxdbh' must be a 'list' object")
    }
    if (is.null(names(maxdbh))) {
      cli::cli_abort("Elements in list 'maxdbh' are missing")
    }

    if (verbose) {
      cli::cli_alert_info("[{fn_name}] Setting attribute 'maxdbh'")
    }

    attr(sf_plots, "maxdbh") <- maxdbh
  }


  if (!is.null(crs)) {
    if (!is_valid_epsg(crs)) {
      cli::cli_abort("If 'crs' is not NULL, it must be a valid CRS code")
    } else {

      if (verbose) {
        cli::cli_alert_info("[{fn_name}] Setting attribute 'crs'")
      }

      sf::st_crs(sf_plots) <- crs
    }
  }


  return(sf_plots)
}
