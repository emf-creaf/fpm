#' Title
#'
#' @description
#' This function calculates the survival term in the "IPM" methodology
#' as a function of dbh.
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
#' @export
#'
#' @examples
#' See Vignettes.
fpm_survival <- function(a, type = "", data = data.frame(), models = list(), verbose = T) {


  # Retrieve parameters.
  p <- get_parameters(a, c("country", "integvars", "mindbh", "maxdbh"))
  country <- p$country
  x <- p$integvars
  nx <- lapply(x, length)


  # If verbose is TRUE, print a progress bar.
  if (verbose) {
    fname <- as.character(match.call()[[1]])
    cat(paste0("\n -> ", fname, ": Calculating tree survival per plot...\n"))
    pb <- utils::txtProgressBar(min = 0,
                                max = length(a$idplot),
                                style = 3,
                                width = 50,
                                char = "=")
  }


  # Add statistics and species to 'data', assuming (and not checking) that calc_stats and calc_species
  # have already been applied.
  if (country == "spain") {
    b <- sf::st_drop_geometry(a)
    b[, c("idplot", "stand_type", "date", "trees", "saplings", "seedlings",
          "ba_species", "ntrees_species", "species", "species_all", "nspecies")] <- NULL
    data <- cbind(data, b)
  }


  # First initialize stands.
  b <- clear_stands(a)


  # Extract info from a.
  icount = 0
  for (i in 1:nrow(a)) {


    # Progress bar.
    icount <- icount + 1
    if (verbose) setTxtProgressBar(pb, icount)


    if (country == "spain") {

      # Calculate only if "ipm".
      if (a[i, ]$stand_type == "ipm") {

        # Species names of adult trees already present in the plot.
        sp <- names(a[i, ]$trees[[1]])

        #Do nothing if there are no trees.
        if (length(sp) > 0) {

          pr <- list()

          for (k in sp) {
            df <- rep_dataframe(data[i, ], nx[[k]])
            df$dbh <- x[[k]]
            pr[[k]] <- predict(models[["survival"]][[k]], type = "response", newdata = df)
          }
        }


        # Save in sf.
        b[i, ]$trees[[1]] <- pr

      }
    }

  }

  if (verbose) cat("\n")

  return(b)


}
