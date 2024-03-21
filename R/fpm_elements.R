#' Calculate the different parts of an ipm model
#'
#' @description
#' It checks inputs and calls other functionsn to calculate "IPM" elements.
#'
#' @param a a \code{sf} object containing a number of POINT geometry types.
#' @param type \code{character} indicating the component of the ipm model to be calculated. See
#' @param data \code{list} whose elements are required to calculate the \code{type} component.
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
fpm_elements <- function(a, type = "", data = data.frame(), models = list(), verbose = T, flag = flag) {


  # Retrieve country.
  country <- get_parameters(a, "country")$country


  # Input 'data' list must be provided.
  stopifnot("Input 'data' cannot be empty" = nrow(data) > 0)
  stopifnot("Input 'data' must be a data.frame" = inherits(data, "data.frame"))


  # If idplot identifier in 'a' and 'data' do not match exactly, stop.
  stopifnot("Index 'idplot' in a' and 'data' do not match exactly" = identical(a$idplot, data$idplot))


  # Computations for spain.
  if (country == "spain") {

    # type must be seedlings, saplings or trees.
    stopifnot("Input 'type' must be a character" = is.character(type))
    typename <- c("seedlings", "saplings", "ingrowth", "survival", "growth")
    stopifnot("Wrong 'type' value for 'country' = 'spain'" = any(type %in% typename))


    # Names of models are ok.
    modelname <- c("seedlings", "saplings", "ingrowth", "lambda", "survival", "growth")
    stopifnot("Missing models" = all(sapply(modelname, function(x) x %in% names(models))))


    # Do calculations.
    b <- switch(type,
                seedlings = fpm_small(a, type = type, data = data, models = models, verbose = verbose),
                saplings = fpm_small(a, type = type, data = data, models = models, verbose = verbose),
                ingrowth = fpm_small(a, type = type, data = data, models = models, verbose = verbose),
                survival = fpm_survival(a, data = data, models = models, verbose = verbose),
                growth = fpm_growth(a, data = data, models = models, verbose = verbose, flag = flag)

    )

  }  else if (country == "usa") {
    stop("Calculations for country = 'usa' have not yet been implemented")
  } else if (country == "france") {
    stop("Calculations for country = 'france' have not yet been implemented")
  }


  return(b)

}
