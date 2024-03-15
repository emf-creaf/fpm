#' Title
#'
#' @param a
#' @param type
#' @param data
#' @param models
#' @param verbose
#'
#' @return
#' @export
#'
#' @examples
fpm_survival <- function(a, type = "", data = data.frame(), models = list(), verbose = T) {


  # Retrieve parameters.
  p <- get_parameters(a, c("country", "integvars", "min_dbh", "max_dbh"))
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
