#' Compute number of small trees.
#'
#' @description
#' It calculates the number of small trees for each class, as well as the number and
#' distribution of ingrowth trees.
#'
#' @param a a \code{sf} object containing a number of POINT geometry types.
#' @param type \code{character} indicating the component of the ipm model to be calculated.
#' @param data \code{list} whose elements are required to calculate the \code{type} component.
#' @param models names \code{list} with the regressions models to calculate the dynamics
#' of small individuals.
#' @param verbose \code{logical}, if set to TRUE a progress bar will be printed.
#'
#' @return
#' A new \code{sf} stand object with the corresponding calculations.
#'
#' @details
#' This function is executed by function "fpm" and should usually not be used directly.
#'
#' @export
#'
#' @examples
#' See Vignettes.
#'
#'
#'
#'
#'

fpm_small <- function(sf_plots = sf_plots, type = NULL, df = NULL, lst_models = lst_models, verbose = TRUE) {

  # Checks.
  if (!inherits(sf_plots, "sf")) {
    cli::cli_abort("Parameter 'sf_plots' must be a 'sf' object")
  }

  if (!is.character(type) || length(type) != 1 || is.null(type)) {
    cli::cli_abort("Wrong 'type' character parameter. It must be 'seedlings', 'saplings' or 'ingrowth'")
  }

  if (!is.null(df)) {
    cli::cli_abort("Wrong 'df' parameter")
  }

  .fpm_small(sf_plots = sf_plots,
             type = type,
             data = df,
             lst_models = check_list_of_models(lst_models),
             verbose = verbose)

}


.fpm_small <- function(sf_plots, type = "", data = data.frame(), lst_models = lst_models, verbose = T) {


  # Retrieve parameters.
  p <- get_parameters(sf_plots, c("country", "integvars", "h"))
  country <- p$country
  if (type == "ingrowth") {
    x <- p$integvars
    h <- p$h
  }


  # Add statistics and species to 'df', assuming (and not checking) that calc_stats and calc_species
  # have already been applied.
  if (country == "spain") {
    b <- sf::st_drop_geometry(sf_plots)
    b[, c("idplot", "stand_type", "date", "trees", "saplings", "seedlings")] <- NULL
    data <- cbind(data, b)
  }


  # A new empty 'sf' is required to store results.
  b <- clear_stands(sf_plots)


  # If verbose is TRUE, print a progress bar.
  if (verbose) {
    fname <- as.character(match.call()[[1]])
    cat(paste0("\n -> ", fname, ": Calculating ", type, " per plot...\n"))
    pb <- utils::txtProgressBar(min = 0,
                                max = length(sf_plots$idplot),
                                style = 3,
                                width = 50,
                                char = "=")
  }


  # Internal functions.
  ifNULLzero <- function(z) ifelse(is.null(z), 0, z)

  getn <- function(z, q) {
    zz <- if (length(z) > 0) {
      j <- which(z$species == q)
      ifelse(length(j) > 0, z$n[j], 0)
    } else 0
    return(zz)
  }


  # Extract info from a.
  icount <- 0
  for (i in 1:nrow(sf_plots)) {


    # Progress bar.
    icount <- icount + 1
    if (verbose) setTxtProgressBar(pb, icount)


    if (country == "spain") {

      if (any(type %in% c("seedlings", "saplings", "ingrowth"))) {

        sp <- switch(type,
                     seedlings = sf_plots[i, ]$species[[1]]$seedlings,
                     saplings = sf_plots[i, ]$species[[1]]$saplings,
                     ingrowth = sf_plots[i, ]$species_all[[1]]
        )

        # Only if there are species.
        if (length(sp) > 0) {

          # Initialize empty data.frame
          if (any(type %in% c("seedlings", "saplings"))) {
            nn <- data.frame(species = character(), n = numeric())
          } else {
            nn <- list()
          }
# if (type == "saplings" & i == 54) browser()
          for (k in sp) {

            # Prepare the data.frame and select species.
            dat <- data[i, ]

            dat$ba <- ifNULLzero(dat$ba)
            dat$nseedlings <- getn(a[i, ]$seedlings[[1]], k)
            if (any(type %in% c("seedlings", "saplings"))) dat$ntrees_species <- ifNULLzero(dat$ntrees_species[[1]][[k]])
            if (any(type %in% c("saplings", "ingrowth"))) dat$nsaplings <- getn(a[i, ]$saplings[[1]], k)

            # Prediction.
            p <- NULL
            if (type == "seedlings") {
              p = predict(lst_models[["seedlings"]][[k]], type = "response", newdata = dat)
            } else if (type == "saplings") {
              if (dat$ntrees_species > 0 | dat$nseedlings > 0) {
                p = predict(lst_models[["saplings"]][[k]], type = "response", newdata = dat)
              }
            } else if (type == "ingrowth") {
              if (dat$nseedlings > 0 | dat$nsaplings > 0) {
                p <- predict(lst_models[["ingrowth"]][[k]], newdata = dat, type = "response") *
                  dtrexp(x = x[[k]], rate = lst_models[["lambda"]][[k]], min = min(x[[k]]))
                p <- p * (10000/(pi*25))
              }
            }

            # Save.
            if (!is.null(p)) {
              if (any(type %in% c("seedlings", "saplings"))) {
                nn <- rbind(nn, data.frame(species = k, n = p))
              } else if (type == "ingrowth") {
                nn[[k]] <- p
              }
            }
          }


          # Store in 'sf'.
          if (type == "seedlings") {
            b[i, ]$seedlings[[1]] <- nn
          } else if (type == "saplings") {
            b[i, ]$saplings[[1]] <- nn
          } else if (type == "ingrowth") {
            b[i, ]$trees[[1]] <- nn
          }
        }
      }
    }
  }


  if (verbose) cat("\n")


  return(b)
}
