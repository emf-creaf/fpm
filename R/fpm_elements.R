#' Calculate the different parts of an ipm model
#'
#' @details
#'
#' @param a a \code{sf} object containing a number of POINT geometry types.
#' @param type \code{character} indicating the component of the ipm model to be calculated. See
#' @param data \code{list} whose elements are required to calculate the \code{type} component.
#' @param verbose logical, if set to TRUE a progress bar will be printed.
#'
#' @description
#' Most checks are done by other functions.
#'
#' @return
#' A \code{sf} object containing the desired "ipm" component per plot.
#' @export
#'
#' @examples
fpm_elements <- function(a, type = "", data = list(), verbose = T) {


  # Retrieve country.
  country <- get_parameters(a, "country")$country


  # Input 'data' list must be provided.
  stopifnot("Input 'data' cannot be empty" = length(data) > 0)
  stopifnot("Input 'data' must be a list" = inherits(data, "list"))


  # If idplot identifier in 'a' and 'df' do not match exactly, stop.
  stopifnot("Index 'idplot' in a' and 'df' do not match exactly" = identical(a$idplot, df$idplot))


  # Computations for spain.
  if (country == "spain") {


    # type must be seedlings, saplings or trees.
    stopifnot("Input 'type' must be a character" = is.character(type))
    stopifnot("Wrong 'type' value for 'country' = 'spain'" = any(type %in% c("seedlings", "saplings", "trees")))


    # Names of models are ok.
    modelname <- c("seedlings_model", "saplings_model", "ingrowth_model", "ingrowth_lambda", "growth_model", "survival_model")
    stopifnot("Missing models" = all(sapply(modelname, function(x) x %in% names(data$models_list))))


    # Do calculations.
    b <- switch(type,
                seedlings = fpm_seedlings(a, data = data, verbose = verbose),
                saplings = fpm_saplings(a, data = data, verbose = verbose),
                ingrowth = fpm_ingrowth(a, data = data, verbose = verbose)
    )

  }  else if (country == "usa") {
    stop("Calculations for country = 'usa' have not yet been implemented")
  } else if (country == "france") {
    stop("Calculations for country = 'france' have not yet been implemented")
  }


  return(b)

}
