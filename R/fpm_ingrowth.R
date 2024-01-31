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
fpm_ingrowth <- function(a, df, models_list,  statistics = NULL, species = NULL, verbose = T) {


  # First checks.
  stopifnot("Input 'a' must be an sf object" = inherits(a, "sf"))
  stopifnot("Input 'df' must be a data.frame" = is.data.frame(df))


  # If idplot identifier in 'a' and 'df' do not match exactly, stop.
  stopifnot("Index 'idplot' in a' and 'df' do not match exactly" = identical(a$idplot, df$idplot))


  # 'stand_type' must be 'ipm'.
  stopifnot("'stand_type' must be 'ipm' in all cases" = all(a$stand_type == "ipm"))


  # Which country' inventory is it?
  country <- match.arg(tolower(attr(a, "country")), c("spain", "france", "usa"))


  # Fetch models.
  ingrowth_lambda <- models_list[["ingrowth_lambda"]]
  ingrowth_model <- models_list[["ingrowth_model"]]


  # Abscissas per species.
  x <- get_parameters(a, "integvar")


  # Get stats and species per plot.
  if (is.null(statistics) | !inherits(statistics, "sf")) statistics <- get_stats(a, verbose = verbose)
  if (is.null(species) | !inherits(species, "sf")) species <- get_species(a, verbose = verbose)


  # First initialize stands.
  ingr_sf <- clear_stands(a)


  # If verbose is TRUE, print a progress bar.
  if (verbose) {
    fname <- as.character(match.call()[[1]])
    cat(paste0("\n -> ", fname, ": Calculating ingrowth per plot...\n"))
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
      if (b$stand_type =="ipm") {

        # Species loop for this plot.
        dat <- df[i, ]
        dat$ba <- statistics$ba[i]

        for (k in species$species_all[[i]]) {

          dat$ntrees_species <- if (length(statistics$ntrees_species[[i]])>0) with(statistics$ntrees_species[[i]], ntrees[species == k]) else 0
          dat$nsaplings <- if (length(b$saplings[[1]])>0) with(b$saplings[[1]], n[species == k]) else 0
          dat$nseedlings <- if (length(b$seedlings[[1]])>0) with(b$seedlings[[1]], n[species == k]) else 0
          if (dat$nseedlings > 0 | dat$nsaplings > 0) {
            p <- 1/(1+exp(-predict(ingrowth_model[[k]], newdata = dat)))
            ingr_sf[i, ]$trees[[1]][[k]] <- dat$nsaplings * p *
              dtrexp(x = x[[k]], rate = ingrowth_lambda[[k]], min = min(x[[k]]))
          }
        }
      }
    }
  }

  if (verbose) cat("\n")

  return(ingr_sf)

}
