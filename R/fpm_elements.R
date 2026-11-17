#' Calculate the different parts of an ipm model
#'
#' @description
#' It checks inputs and calls other functionsn to calculate "IPM" elements.
#'
#' @param a a \code{sf} object containing a number of POINT geometry types.
#' @param type \code{character} indicating the component of the ipm model to be calculated. See
#' @param df \code{list} whose elements are required to calculate the \code{type} component.
#' @param models a \code{list} containing regression models per species for all
#' the IPM steps.
#' @param verbose logical, if set to TRUE a progress bar will be printed.
#'
#' @details
#' This function calls, from within "fpm", the selected IPM element, i.e. "fpm_small", "fpm_survival" or
#' "fpm_growth". It should usually not be used directly.
#'
#' @return
#' A \code{sf} object containing the desired "ipm" component per plot.
#'
#' @export
#'
fpm_elements <- function(sf_plots = NULL, type = NULL, df = NULL, lst_models = NULL, verbose = TRUE) {


  # Checks.
  check_param_fpm(sf_plots = sf_plots, type = type, df = df, lst_models = lst_models)


  # Retrieve country.
  country <- get_parameters(a, "country")$country


  if (verbose) {
    fn_name <- as.character(sys.call()[[1]])
    cat("\n")
  }


  # Do calculations.
  if (verbose) {
    cli::cli_alert_info("[{fn_name}] Calculating ", type)
  }


  b <- switch(type,
              seedlings = .fpm_small(a, type = type, df = df, lst_models = lst_models, verbose = verbose),
              saplings = .fpm_small(a, type = type, df = df, lst_models = lst_models, verbose = verbose),
              ingrowth = .fpm_small(a, type = type, df = df, lst_models = lst_models, verbose = verbose),
              survival = .fpm_survival(a, df = df, lst_models = lst_models, verbose = verbose),
              growth = .fpm_growth(a, df = df, lst_models = lst_models, verbose = verbose)
  )


  if (verbose) {
    cat("\n")
  }


  return(b)

}
