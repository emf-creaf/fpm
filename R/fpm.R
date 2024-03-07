#' Implementation of a forest dynamics model
#'
#' @details
#' \code{fpm} implements all steps of a full forest dynamics model, including
#' seedling, sapling, ingrowth and adult-tree growth and mortality processes.
#' @param a
#' @param data
#' @param models
#' @param verbose
#'
#' @return
#' @export
#'
#' @examples
fpm <- function(a, data = data.frame(), models = data.frame(), verbose = T) {


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
  stopifnot("Index 'idplot' in 'a' and 'data' must match exactly" = identical(a$idplot, data$idplot))


  # Check models is not NULL or empty.
  stopifnot("Input 'models' must be provided" = !is.null(models))
  stopifnot("Input 'models' must be a list" = inherits(models, "list"))
  stopifnot("Input 'models' cannot be empty" = length(models) > 0)


  # Get stats and species per plot.
  a <- a |> get_species(verbose = verbose) |> get_stats(verbose = verbose)


  # Compute young and ingrowth trees.
  seedlings <- fpm_elements(a, "seedlings", data = data, models = models, verbose = verbose)
  saplings <- fpm_elements(a, "saplings", data = data, models = models, verbose = verbose)
  ingrowth <- fpm_elements(a, "ingrowth", data = data, models = models, verbose = verbose)
  survival <- fpm_elements(a, "survival", data = data, models = models, verbose = verbose)


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


  # Save trees in 'sf' object.
  adults <- clear_stands(a)


  icount = 0
  for (i in 1:nrow(adults)) {


    # Progress bar.
    icount <- icount + 1
    if (verbose) setTxtProgressBar(pb, icount)


    # Numerical quadrature with extended Simpson' rule. Run only if there are trees.
    if (country == "spain") {
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


  # Collect everything and build the new 'sf' object.
  b <- collect_parts(seedlings, saplings, ingrowth, adults, verbose = verbose)


  # Extra carriage return.
  if (verbose) {
    t2 <- proc.time()[3]
    cat(paste0("\n -> ", fname, ": Total elapsed time = ", round(t2-t1)," seconds\n"))
    cat("\n")
  }

  return(b)


}
