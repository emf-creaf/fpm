#' Title
#'
#' @param a
#' @param df
#' @param models_list
#' @param statistics
#' @param species
#' @param verbose
#'
#' @return
#'
#' @examples
fpm_saplings <- function(a, data = list(), verbose = T) {




  # Retrieve country.
  country <- get_parameters(a, "country")$country


  # Retrieve data elements.
  df <- data$df
  models_list <- data$models_list


  # Add statistics and species to 'df', assuming (and not checking) that get_stats and get_species
  # have already been applied.
  if (country == "spain") {
    b <- sf::st_drop_geometry(a)
    b[, c("idplot", "stand_type", "date", "trees", "saplings", "seedlings")] <- NULL
    df <- cbind(df, b)
  }


  # Fetch model.
  modl <- models_list[["saplings_model"]]


  # First initialize stands.
  b <- clear_stands(a)


  # If verbose is TRUE, print a progress bar.
  if (verbose) {
    fname <- as.character(match.call()[[1]])
    cat(paste0("\n -> ", fname, ": Calculating seedlings per plot...\n"))
    pb <- utils::txtProgressBar(min = 0,
                                max = length(a$idplot),
                                style = 3,
                                width = 50,
                                char = "=")
  }


  # Silly function.
  ifNULLzero <- function(z) ifelse(is.null(z), 0, z)


  # Extract info from a.
  icount = 0
  for (i in 1:nrow(a)) {


    # Progress bar.
    icount <- icount + 1
    if (verbose) setTxtProgressBar(pb, icount)


    if (country == "spain") {

      b <- a[i, ]
      if (b$stand_type == "ipm") {

        # Species loop for this plot.
        dat <- df[i, ]
        dat$ba <- statistics$ba[i]
        nsee <- nsap <- data.frame(species = character(), n = numeric())

        for (k in species$species_all[[i]]) {
          dat$ntrees_species <- if (length(statistics$ntrees_species[[i]])>0) with(statistics$ntrees_species[[i]], ntrees[species == k]) else 0
          dat$nseedlings <- if (length(b$seedlings[[1]])>0) with(b$seedlings[[1]], n[species == k]) else 0
          dat$nsaplings <- if (length(b$saplings[[1]])>0) with(b$saplings[[1]], n[species == k]) else 0
          if (dat$ntrees_species > 0 | dat$nseedlings > 0) {
            nsap <- rbind(nsap, data.frame(species = k, n = predict(saplings_model[[k]], type = "response", newdata = dat)))
          }
        }
      }
    }

    # Store.

    sapling_sf[i, ]$saplings[[1]] <- nsap

  }

  if (verbose) cat("\n")

  return(sapling_sf)

}
