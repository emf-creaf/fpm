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
#' @export
#'
#' @examples
fpm_seedlings <- function(a, data = list(), verbose = T) {


  # Which country is it?
  country <- get_parameters(a, "country")$country


  # Retrieve data elements.
  df <- data$df
  models_list <- data$models_list
  statistics <- data$statistics
  species <- data$species


  # Add statistics and species to 'df', assuming (and not checking) that get_stats and get_species
  # have already been applied.
  b <- sf::st_drop_geometry(a)
  if (country == "spain") {
    b[, c("idplot", "stand_type", "date", "trees", "saplings", "seedlings")] <- NULL
    df <- cbind(df, b)
  }


  # Fetch model.
  seedlings_model <- models_list[["seedlings_model"]]


  # First initialize stands.
  seedling_sf <- clear_stands(a)


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


  # Extract info from a.
  icount = 0
  for (i in 1:nrow(a)) {


    # Progress bar.
    icount <- icount + 1
    if (verbose) setTxtProgressBar(pb, icount)


    if (country == "spain") {
      b <- a[i, ]
      sp <- b$species[[1]]$seedlings

      if (length(sp) > 0) {
        # Species loop for this plot.
        dat <- df[i, ]
        nsee <- data.frame(species = character(), n = numeric())
browser()
        for (k in sp) {
          dat$nseedlings <- with(b$seedlings[[1]], n[species == k])

browser()
          nsee <- rbind(nsee, data.frame(species = k, n = predict(seedlings_model[[k]], type = "response", newdata = dat)))
        }

        # Store.
        if (nrow(nee) > 0) seedling_sf[i, ]$seedlings[[1]] <- nsee

      }
    }



  }


  if (verbose) cat("\n")

  return(seedling_sf)

}
