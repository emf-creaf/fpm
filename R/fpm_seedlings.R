#' Title
#'
#' @param a
#' @param data \code{list} whose elements are required to calculate the \code{type} component.
#' @param verbose logical, if set to TRUE warning messages may be printed.
#'
#' @return
#'
#' @examples
fpm_seedlings <- function(a, data = list(), verbose = T) {


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
  modl <- models_list[["seedlings_model"]]


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

      sp <- a[i, ]$species[[1]]$seedlings

      if (length(sp) > 0) {

        # Initialize empty data.frame
        nsee <- data.frame(species = character(), n = numeric())

        for (k in sp) {

          # Prepare the data.frame and select species.
          dat <- df[i, ]
          dat$ntrees_species <- ifNULLzero(dat$ntrees_species[[1]][[k]])
          dat$ba_species <- ifNULLzero(dat$ba_species[[1]][[k]])
          dat$nseedlings <- with(a[i, ]$seedlings[[1]], n[species == k])

          # Predict the new number of seedlings.
          nsee <- rbind(nsee, data.frame(species = k, n = predict(modl[[k]], type = "response", newdata = dat)))

        }

        # Store.
        if (nrow(nsee) > 0) b[i, ]$seedlings[[1]] <- nsee

      }
    }
  }


  if (verbose) cat("\n")

  return(b)

}
