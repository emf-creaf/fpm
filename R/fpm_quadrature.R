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
fpm_quadrature <- function(a, df, models_list,  statistics = NULL, verbose = T) {


  # First checks.
  stopifnot("Input 'a' must be an sf object" = inherits(a, "sf"))
  stopifnot("Input 'df' must be a data.frame" = is.data.frame(df))


  # If idplot identifier in 'a' and 'df' do not match exactly, stop.
  stopifnot("Index 'idplot' in a' and 'df' do not match exactly" = identical(a$idplot, df$idplot))


  # Which country' inventory is it?
  country <- match.arg(tolower(attr(a, "country")), c("spain", "france", "usa"))


  # Abscissas per species.
  h <- get_parameters(a, "h")


  # Get stats and species per plot.
  if (is.null(statistics) | !inherits(statistics, "sf")) statistics <- get_stats(a, verbose = F)


  # First initialize stands.
  quadr <- clear_stands(a)


  # Survival is calculated at once, but the growth kernel is done plot by plot.
  su <- fpm_survival(a, df, models_list, statistics, verbose = F)$psurv[[1]]


  # If verbose is TRUE, print a progress bar.
  if (verbose) {
    fname <- as.character(match.call()[[1]])
    cat(paste0("\n -> ", fname, ": Applying quadrature to solve integral numerically...\n"))
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


    # Species loop for this plot. Do nothing if there are no trees.
    b <- a[i, ]
    if (b$stand_type == "ipm") {
      sp_trees <- names(b$trees[[1]])
      if (length(sp_trees) > 0) {


        # Growth kernel.
        gr <- fpm_growth(b, df[i, ], models_list, statistics, verbose = F)$growth[[1]]


        # Numerical quadrature with extended Simpson' rule.
        for (j in sp_trees) {
          b$trees[[1]][[j]] <- numquad_vm(b$trees[[1]][[j]] * su[[j]], gr[[j]], h[[j]], "trapez")
        }
      }


      # Save in new stand.
      quadr$trees[[i]] <- b$trees[[1]]
    }
  }

  if (verbose) cat("\n")

  return(quadr)


}
