#' Title
#'
#' @param a
#' @param df
#' @param verbose
#' @param models_list
#' @param statistics
#'
#' @return
#' @export
#'
#' @examples
fpm_growth <- function(a, df, models_list,  statistics = NULL, verbose = T) {


  # First checks.
  stopifnot("Input 'a' must be an sf object" = inherits(a, "sf"))
  stopifnot("Input 'df' must be a data.frame" = is.data.frame(df))


  # If idplot identifier in 'a' and 'df' do not match exactly, stop.
  stopifnot("Index 'idplot' in a' and 'df' do not match exactly" = identical(a$idplot, df$idplot))


  # Which country' inventory is it?
  country <- match.arg(tolower(attr(a, "country")), c("spain", "france", "usa"))


  # Abscissas per species.
  x <- get_parameters(a, "integvar")
  nx <- lapply(x, length)
  min_dbh <- get_parameters(a, "min_dbh")
  max_dbh <- get_parameters(a, "max_dbh")


  # Fetch models.
  growth_model <- models_list[["growth_model"]]
  variance_model <- models_list[["variance_model"]]


  # Get stats and species per plot.
  if (is.null(statistics) | !inherits(statistics, "sf")) statistics <- get_stats(a, verbose = F)


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
  growth_sf <- clear_stands(a)
  growth_sf$growth <- vector("list", length = nrow(growth_sf))


  # Extract info from a.
  icount = 0
  for (i in 1:nrow(a)) {


    # Progress bar.
    icount <- icount + 1
    if (verbose) setTxtProgressBar(pb, icount)


    # Species loop for this plot. Do nothing if there are no trees.
    b <- a[i, ]
    if (b$stand_type == "ipm") {
      sp_trees <- names(b$trees[[1]])
      if (length(sp_trees) > 0) {

        dat <- df[i, ]
        gr <- list()

        for (j in sp_trees) {

          # data.frame with data for prediction.
          dat$ba <- statistics$ba[i]
          df <- rep_dataframe(dat, length(x[[j]]))
          df$y1 <- x[[j]]
          df$max_y <- max_dbh[[j]]

          meanlog <- predict(growth_model[[j]], newdata = df)
          sdlog <- sqrt(predict(variance_model[[j]], newdata = df))

          mat <- matrix(0, nx[[j]], nx[[j]])
          xx <- x[[j]] - min_dbh[[j]]
          kseq <- 1:nx[[j]]
          for (k in 1:nx[[j]]) {
            mat[k, kseq] <- dlnorm(xx, meanlog = meanlog[k], sdlog = sdlog[k])
            xx <- xx[-length(xx)]
            kseq <- kseq[-1]
          }

          gr[[j]] <- mat

        }

        # Save in sf.
        growth_sf[i, ]$growth[[1]] <- gr

      }
    }
  }

  if (verbose) cat("\n")

  return(growth_sf)


}
