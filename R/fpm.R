#' Implementation of a forest dynamics model
#'
#' @details
#' \code{fpm} implements all steps of a full forest dynamics model, including
#' seedling, sapling, ingrowth and adult-tree growth and mortality processes.
#'
#'
#' @param a
#' @param df
#' @param verbose
#' @param models_list
#' @param statistics
#' @param species
#'
#' @return
#' @export
#'
#' @examples
fpm <- function(a, data = list(), verbose = T) {


  # Time now.
  if (verbose) t1 <- proc.time()[3]


  # Check sf.
  stopifnot("Input 'a' must be an sf object" = inherits(a, "sf"))


  # Which country is it?
  country <- get_parameters(a, "country")$country
  country <- match.arg(tolower(attr(a, "country")), c("spain", "france", "usa"))


  # data is provided.
  stopifnot("Input 'data' list must be provided" = !is.null(data))
  stopifnot("Input 'data' must be a list" = inherits(data, "list"))


  # Check df in data.
  stopifnot("Input 'df' in list 'data' cannot be NULL" = !is.null(data$df))
  stopifnot("Input 'df' must be a data.frame" = is.data.frame(data$df))
  stopifnot("Input 'df' in list 'data' cannot be empty" = nrow(data$df) > 0)


  # If idplot identifier in 'a' and 'df' do not match exactly, stop.
  stopifnot("Index 'idplot' must be provided in data.frame 'df' of 'data' list" = "idplot" %in% names(data$df))
  stopifnot("Index 'idplot' in a' and 'df' must match exactly" = identical(a$idplot, data$df$idplot))


  # Check models_list is not NULL or empty.
  stopifnot("Input 'models_list' in list 'data' cannot be NULL" = !is.null(data$models_list))
  stopifnot("Input 'models_list' in list 'data' cannot be empty" = nrow(data$models_list) > 0)


  # Get stats and species per plot.
  a <- a |> get_stats(verbose = verbose) |> get_species(verbose = verbose)


  # Abscissas per species.
  p <- get_parameters(a, param = c("integvars", "h", "min_dbh", "max_dbh"))
  x <- p$integvars
  h <- p$h
  nx <- lapply(x, length)
  min_dbh <- p$min_dbh
  max_dbh <- p$max_dbh


  # Compute young and ingrowth trees.
  seedlings <- fpm_elements(a, "seedlings", data = data, verbose = verbose)
browser()
  saplings <- fpm_elements(a, "saplings", data = data, verbose = verbose)
  ingrowth <- fpm_elements(a, "ingrowth", data = data, verbose = verbose)


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
  adult <- clear_stands(a)


  icount = 0
  for (i in 1:nrow(adult)) {


    # Progress bar.
    icount <- icount + 1
    if (verbose) setTxtProgressBar(pb, icount)


    # Numerical quadrature with extended Simpson' rule. Run only if there are trees.
    if (country == "spain") {
      if (length(names(a$trees[[i]])) > 0) {
        adult[i, ] <- fpm_quadrature(a[i, ], df[i, ], models_list, statistics, verbose =  F)
      }
    }
  }
  if (verbose) cat("\n")


  # Collect everything and build the new 'sf' object.
  b <- collect_parts(seedling, sapling, ingrowth, adult, verbose = verbose)


  # Extra carriage return.
  if (verbose) {
    t2 <- proc.time()[3]
    cat(paste0("\n -> ", fname, ": Total elapsed time = ", round(t2-t1)," seconds\n"))
    cat("\n")
  }

  return(b)


}
