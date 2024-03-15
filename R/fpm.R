#' Implementation of a forest dynamics model
#'
#' @description
#' \code{fpm} implements all steps of the Integral Projection Model (IPM) methodology
#' to determine forest dynamics model from inventory data. Model steps include
#' seedlings, saplings, ingrowth and adult-tree growth and mortality processes.
#'
#' @param a \code{sf} object containing a number of POINT geometry types.
#' @param data a \code{data.frame} with the same number of rows, and same \code{idplot}
#' identifier, as 'a', containing the covariables and/or factors that are needed to calculate
#' the components of the IPM model.
#' @param models a \code{list} containing regression models per species for all
#' the IPM steps.
#' @param verbose \code{logical}, if set to TRUE a progress bar will be printed.
#' @param update \code{logical}, if set to TRUE descriptive statistics per plot are calculated
#' at the end of the modelization.
#'
#' @details
#' Function \code{fpm} calculates the different elements of the model and
#' combines them together.
#'
#' @return
#' An 'a' \code{sf} object with the updated forest projection.
#'
#' @references
#' Alberdi, I., Sandoval, V., Condes, S., Cañellas, I., & Vallejo, R. (2016). El Inventario Forestal Nacional español, una herramienta para el conocimiento, la gestión y la conservación de los ecosistemas forestales arbolados. Ecosistemas, 25(3), 88-97.
#'
#' @export
#'
fpm <- function(a, data = data.frame(), models = data.frame(), verbose = T, update = T) {


  # Time now.
  if (verbose) t1 <- proc.time()[3]


  # Check sf.
  stopifnot("Input 'a' must be an sf object" = inherits(a, "sf"))


  # Retrieve parameters.
  p <- get_parameters(a, param = c("country", "integvars", "h", "min_dbh", "max_dbh"))
  country <- match.arg(p$country, c("spain", "france", "usa"))
  x <- p$integvars
  h <- p$h
  nx <- lapply(x, length)
  min_dbh <- p$min_dbh
  max_dbh <- p$max_dbh


  # data is provided.
  stopifnot("Input data.frame 'data' must be provided" = !is.null(data))
  stopifnot("Input 'data' must be a data.frame" = inherits(data, "data.frame"))
  stopifnot("Input data.frame 'data' cannot be empty" = nrow(data) > 0)


  # If idplot identifier in 'a' and 'df' do not match exactly, stop.
  stopifnot("Index 'idplot' must be provided in data.frame 'data'" = "idplot" %in% names(data))
  i <- match(a$idplot, data$idplot)
  stopifnot("Some 'idplot' indices in 'a' cannot be found" = sum(is.na(i)) == 0)


  # Check models is not NULL or empty.
  stopifnot("Input 'models' must be provided" = !is.null(models))
  stopifnot("Input 'models' must be a list" = inherits(models, "list"))
  stopifnot("Input 'models' cannot be empty" = length(models) > 0)


  # Reorder 'data' to match 'a' line by line.
  data <- data[i[!is.na(i)], ]


  # Get stats and species per plot.
  a <- a |> calc_species(verbose = verbose) |> calc_stats(verbose = verbose)


  if (country == "spain") {
    # Compute young and ingrowth trees.
    seedlings <- fpm_elements(a, "seedlings", data = data, models = models, verbose = verbose)
    saplings <- fpm_elements(a, "saplings", data = data, models = models, verbose = verbose)
    ingrowth <- fpm_elements(a, "ingrowth", data = data, models = models, verbose = verbose)
    survival <- fpm_elements(a, "survival", data = data, models = models, verbose = verbose)

    # Save trees in 'sf' object.
    adults <- clear_stands(a)
  }


  # If verbose is TRUE, print a progress bar.
  if (verbose) {
    fname <- as.character(match.call()[[1]])
    cat(paste0("\n -> ", fname, ": Computing forest dynamics per plot...\n"))
    pb <- utils::txtProgressBar(min = 0,
                                max = length(a$idplot),
                                style = 3,
                                width = 50,
                                char = "=")
  }


  icount = 0
  for (i in 1:nrow(a)) {


    # Progress bar.
    icount <- icount + 1
    if (verbose) setTxtProgressBar(pb, icount)


    # Numerical quadrature with extended Simpson' rule.
    # Growth is called once per loop step because of the size of its output.
    if (country == "spain") {

      # IPM dynamics only if there are trees.
      if (length(a$trees[[i]]) > 0) {
        growth <- fpm_elements(a[i, ], "growth", data = data[i, ], models = models, verbose = F)
        adults[i, ] <- fpm_quadrature(a[i, ], verbose =  F,
                                     data = list(survival = survival[i, ], growth = growth))
      }

    }  else if (country == "usa") {
      stop("Calculations for country = 'usa' have not yet been implemented")
    } else if (country == "france") {
      stop("Calculations for country = 'france' have not yet been implemented")
    }
  }
  if (verbose) cat("\n")


  if (country == "spain") {

    # Collect everything and build the new 'sf' object.
    b <- collect_parts(a, list(seedlings = seedlings,
                               saplings = saplings,
                               ingrowth = ingrowth,
                               adults = adults), verbose = verbose)

  }


  # Update species and statistics if set.
  if (update) {
    b <- b |> calc_stats(verbose = verbose) |> calc_species()
  }# Extra carriage return.


  if (verbose) {
    t2 <- proc.time()[3]
    cat(paste0("\n -> ", fname, ": Total elapsed time = ", round(t2-t1)," seconds\n"))
    cat("\n")
  }

  return(b)


}
