#' Compute number of small trees.
#'
#' @description
#' It calculates the number of small trees for each class, as well as the number and
#' distribution of ingrowth trees.
#'
#' @param a a \code{sf} object containing a number of POINT geometry types.
#' @param type \code{character} indicating the component of the ipm model to be calculated.
#' @param data \code{list} whose elements are required to calculate the \code{type} component.
#' @param verbose logical, if set to TRUE a progress bar will be printed.
#'
#' @return
#' A new \code{sf} stand object with the corresponding calculations.
#'
#' @export
#'
#' @examples
fpm_small <- function(a, type = "", data = list(), verbose = T) {


  # Retrieve parameters.
  p <- get_parameters(a, c("country", "integvars", "h"))
  country <- p$country
  if (type == "ingrowth") {
    x <- p$integvars
    h <- p$h
  }


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


  # If type = "ingrowth", a new empty 'sf' is required to store ingrowth trees.
  b <- clear_stands(a)


  # If verbose is TRUE, print a progress bar.
  if (verbose) {
    fname <- as.character(match.call()[[1]])
    cat(paste0("\n -> ", fname, ": Calculating ", type, " per plot...\n"))
    pb <- utils::txtProgressBar(min = 0,
                                max = length(a$idplot),
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
  icount = 0
  for (i in 1:nrow(a)) {


    # Progress bar.
    icount <- icount + 1
    if (verbose) setTxtProgressBar(pb, icount)


    if (country == "spain") {

      if (any(type %in% c("seedlings", "saplings", "ingrowth"))) {

        sp <- switch(type,
                     seedlings = a[i, ]$species[[1]]$seedlings,
                     saplings = a[i, ]$species[[1]]$saplings,
                     ingrowth = a[i, ]$species_all[[1]]
        )

        # Only if there are species.
        if (length(sp) > 0) {

          # Initialize empty data.frame
          if (any(type %in% c("seedlings", "saplings"))) {
            n <- data.frame(species = character(), n = numeric())
          } else {
            n <- list()
          }

          for (k in sp) {

            # Prepare the data.frame and select species.
            dat <- df[i, ]
            dat$ntrees_species <- ifNULLzero(dat$ntrees_species[[1]][[k]])
            dat$ba_species <- ifNULLzero(dat$ba_species[[1]][[k]])
            dat$nseedlings <- getn(a[i, ]$seedlings[[1]], k)
            if (any(type %in% c("saplings", "ingrowth"))) dat$nsaplings <- getn(a[i, ]$saplings[[1]], k)

            # Prediction.
            p <- 0
            if (type == "seedlings") {
              p = predict(models_list[["seedlings_model"]][[k]], type = "response", newdata = dat)
            } else if (type == "saplings") {
              if (dat$ntrees_species > 0 | dat$nseedlings > 0) {
                p = predict(models_list[["saplings_model"]][[k]], type = "response", newdata = dat)
              }
            } else if (type == "ingrowth") {
              if (dat$nseedlings > 0 | dat$nsaplings > 0) {
                p <- predict(models_list[["ingrowth_model"]][[k]], newdata = dat, type = "response") *
                  dtrexp(x = x[[k]], rate = models_list[["ingrowth_lambda"]][[k]], min = min(x[[k]]))
              }
            }

            # Save.
            if (any(type %in% c("seedlings", "saplings"))) {
              n <- rbind(n, data.frame(species = k, n = p))
            } else if (type == "ingrowth") {
              n[[k]] <- p
            }
          }

          # Store in 'sf'.
          if (type == "seedlings") {
            b[i, ]$seedlings[[1]] <- n
          } else if (type == "saplings") {
            b[i, ]$saplings[[1]] <- n
          } else if (type == "ingrowth") {
            b[i, ]$trees[[1]] <- n
          }

        }
      }
    }
  }


  if (verbose) cat("\n")


  return(b)
}
