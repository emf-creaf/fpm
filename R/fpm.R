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
fpm <- function(a, df, models_list,  statistics = NULL, species = NULL, verbose = T) {

  # Time now.
  if (verbose) t1 <- proc.time()[3]


  # First checks.
  stopifnot("Input 'a' must be an sf object" = inherits(a, "sf"))
  stopifnot("Input 'df' must be a data.frame" = is.data.frame(df))


  # If idplot identifier in 'a' and 'df' do not match exactly, stop.
  stopifnot("Index 'idplot' in a' and 'df' do not match exactly" = identical(a$idplot, df$idplot))


  # Which country' inventory is it?
  country <- match.arg(tolower(attr(a, "country")), c("spain", "france", "usa"))


  # Get stats and species per plot.
  if (is.null(species) | !inherits(species, "sf")) species <- get_species(a, verbose = verbose)
  if (is.null(statistics) | !inherits(statistics, "sf")) statistics <- get_stats(a, verbose = verbose)


  # Abscissas per species.
  p <- get_parameters(a, param = c("integvars", "h", "min_dbh", "max_dbh"))
  h <- p$h
  x <- p$integvars
  nx <- lapply(x, length)
  min_dbh <- p$min_dbh
  max_dbh <- p$max_dbh


  # Compute young and ingrowth trees.
  seedling <- fpm_seedling(a, df, models_list, statistics, species, verbose = verbose)
  sapling <- fpm_sapling(a, df, models_list, statistics, species, verbose = verbose)
  ingrowth <- fpm_ingrowth(a, df, models_list, statistics, species, verbose = verbose)


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
