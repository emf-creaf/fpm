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
fpm_survival <- function(a, df, models_list,  statistics = NULL, verbose = T) {


  # First checks.
  stopifnot("Input 'a' must be an sf object" = inherits(a, "sf"))
  stopifnot("Input 'df' must be a data.frame" = is.data.frame(df))


  # If idplot identifier in 'a' and 'df' do not match exactly, stop.
  stopifnot("Index 'idplot' in a' and 'df' do not match exactly" = identical(a$idplot, df$idplot))


  # Which country' inventory is it?
  country <- match.arg(tolower(attr(a, "country")), c("spain", "france", "usa"))


  # Fetch models.
  survival_model <- models_list[["survival_model"]]


  # Get stats and species per plot.
  if (is.null(statistics) | !inherits(statistics, "sf")) statistics <- get_stats(a, verbose = F)


  # First initialize stands.
  surv_sf <- clear_stands(a)
  surv_sf$trees <- surv_sf$saplings <- surv_sf$seedlings <- NULL
  surv_sf$psurv <- vector("list", length = nrow(surv_sf))


  # Abscissas per species.
  x <- get_parameters(a, "integvar")


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


  # Extract info from a.
  icount = 0
  for (i in 1:nrow(a)) {


    # Progress bar.
    icount <- icount + 1
    if (verbose) setTxtProgressBar(pb, icount)


    if (country == "spain") {

      # Species loop for this plot. Do nothing if there are no trees.
      b <- a[i, ]
      if (b$stand_type == "ipm") {
        sp_trees <- names(b$trees[[1]])
        if (length(sp_trees) > 0) {

          dat <- df[i, ]
          dat$ba <- statistics$ba[i]
          psurv <- list()

          for (k in sp_trees) {
            df <- rep_dataframe(dat, length(x[[k]]))
            df$dbh <- x[[k]]
            psurv[[k]] <- predict(survival_model[[k]], type = "response", newdata = df)
          }
        }


        # Save in sf.
        surv_sf[i, ]$psurv[[1]] <- psurv

      }
    }

  }

  if (verbose) cat("\n")

  return(surv_sf)


}
