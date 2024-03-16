#' Calculation of tree growth
#'
#' @description
#' It computes tree dbh growth for all species and plots.
#'
#' @param a a \code{sf} object containing a number of POINT geometry types.
#' @param data \code{list} whose elements are required to calculate growth.
#' @param models a \code{list} containing regression models per species for all
#' the IPM steps.
#' @param verbose \code{logical}, if set to TRUE a progress bar will be printed.
#'
#' @return
#' A \code{sf} object with the projected tree population.
#' @export
#'
#' @examples
#' See Vignettes.
fpm_growth <- function(a, data = data.frame(), models = list(), verbose = T) {


  # Retrieve parameters.
  p <- get_parameters(a, c("country", "integvars", "min_dbh", "max_dbh"))
  country <- p$country
  x <- p$integvars
  nx <- lapply(x, length)
  min_dbh <- p$min_dbh
  max_dbh <- p$max_dbh


  # Fetch models.
  growth <- models[["growth"]]
  variance <- models[["variance"]]


  # If verbose is TRUE, print a progress bar.
  if (verbose) {
    fname <- as.character(match.call()[[1]])
    cat(paste0("\n -> ", fname, ": Calculating tree growth per plot...\n"))
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


    # Calculate only if "ipm".
    if (a[i, ]$stand_type == "ipm") {

      # Species names of adult trees already present in the plot.
      sp <- names(a[i, ]$trees[[1]])

      # Do nothing if there are no trees.
      if (length(sp) > 0) {


        # To store predictions.
        z <- list()


        # Species loop.
        for (j in sp) {

          dat <- rep_dataframe(data[i, ], nx[[j]])
          dat$y1 <- x[[j]]
          dat$max_y <- max_dbh[[j]]

          meanlog <- predict(growth[[j]], newdata = dat)
          sdlog <- sqrt(predict(variance[[j]], type = "response", newdata = dat))

          mat <- matrix(0, nx[[j]], nx[[j]])
          xx <- x[[j]] - min_dbh[[j]]
          kseq <- 1:nx[[j]]

          for (k in 1:nx[[j]]) {
            mat[k, kseq] <- dlnorm(xx, meanlog = meanlog[k], sdlog = sdlog[k])
            xx <- xx[-length(xx)]
            kseq <- kseq[-1]
          }

          z[[j]] <- mat

        }

        # Save in sf.
        b[i, ]$trees[[1]] <- z

      }
    }
  }

  if (verbose) cat("\n")


  return(b)
}
