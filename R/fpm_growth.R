#' Calculation of tree growth
#'
#' @description
#' It computes tree dbh growth for all species and plots.
#'
#' @param a
#' @param df
#' @param verbose
#'
#' @return
#' @export
#'
#' @examples
fpm_growth <- function(a, type = "", data = list(), verbose = T) {


  # Only works for "ipm" plots.
  stopifnot("Tree growth can only be calculated for 'ipm' stands" = all(a$stand_type == "ipm"))

  # Retrieve parameters.
  p <- get_parameters(a, c("country", "integvars", "min_dbh", "max_dbh"))
  country <- p$country
  x <- p$integvars
  nx <- lapply(x, length)
  min_dbh <- p$min_dbh
  max_dbh <- p$max_dbh


  # Fetch models.
  growth_model <- models_list[["growth_model"]]
  variance_model <- models_list[["variance_model"]]


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


  # First initialize stands.
  b <- clear_stands(a)


  # Extract info from a.
  icount = 0
  for (i in 1:nrow(a)) {


    # Progress bar.
    icount <- icount + 1
    if (verbose) setTxtProgressBar(pb, icount)


    # Species loop for this plot. Do nothing if there are no trees.
    if (a[i, ]$stand_type == "ipm") {

      # Species names of adult trees already present in the plot.
      sp <- names(b$trees[[1]])

      if (length(sp) > 0) {


        # data.frame with data for prediction.
        dat <- data$df[i, ]
        z <- list()

        for (j in sp) {

          dat$y1 <- x[[j]]
          dat$max_y <- max_dbh[[j]]

          meanlog <- predict(growth_model[[j]], type = "response", newdata = dat)
          sdlog <- sqrt(predict(variance_model[[j]], type = "response", newdata = dat))

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
