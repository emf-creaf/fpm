#' Convolution of discrete tree stands
#'
#' @description
#' Convolution of discrete tree stands per species with a smoothing window.
#'
#' @param a a \code{sf} object containing a number of POINT geometry types.
#' @param smooth_type string indicating which smoothing window to use. Presently,
#' only \code{smooth_type = "gaussian"} option is available.
#' @param verbose logical, if set to TRUE a progress bar will be printed on screen.
#' @param width width of smoothing window. Default is 2.
#'
#' @return
#' A \code{sf} object with a continuous distributions of trees per species as a
#' function of dbh, instead of a set of individual dbh values (as would happen
#' if data came from observations).
#'
#' @details
#' Convolution with a continuous window function is done with a numerical quadrature.
#' The default value (\code{smooth_type = "gaussian"}) such be good enough for
#' most purposes.
#'
#' @export
#'
#' @examples
#'
#' a <- start_stands()
#' max_dbh <- list('Pinus halepensis' = 200, 'Pinus nigra' = 230)
#' a <- set_parameters(a, param = list(max_dbh = max_dbh, crs =  "EPSG:4326"))
#'
#' # Next, we add one stand.
#' df <- data.frame(species = c('Pinus halepensis', 'Quercus ilex'), dbh = c(8.6, 12.7))
#' a <- build_stand(a, "id1", data = list(df = df), verbose = T)
#'
#' # Convolve every tree in every plot with a Gaussian window.
#' b <- smooth_stands(a)
#'
smooth_stands <- function(a, smooth_type = "gaussian", width = 2, verbose = T) {

  # Check that input 'a' is an 'sf' object.
  stopifnot("Input 'a' must be an sf object" = inherits(a, "sf"))


  mf <- match.call()
  m <- match(c("a", "idplot", "smooth_type", "width", "verbose"), tolower(names(mf)[-1]))


  # We need the integration variable for the calculations if any stand is "ipm".
  x <- get_parameters(a, "integvars")$integvars
  h <- get_parameters(a, "h")$h
  stopifnot("Attribute 'integvars' is missing" = !is.null(x))


  # Check country.
  country <- match.arg(attr(a, "country"), c("spain", "france", "usa"))


  # If progress is TRUE, print a progress bar.
  if (verbose) {
    cat("\n -> smooth_stands: Transforming discrete tree data to continuous...\n")
    pb <- txtProgressBar(min = 0,
                         max = length(id),
                         style = 3,
                         width = 50,
                         char = "=")
  }

  # Loop along all plots.
  icount <- 0
  for (i in 1:nrow(a)) {

    # Progress bar.
    icount <- icount + 1
    if (verbose) setTxtProgressBar(pb, icount)


    # Calculate only if there are trees.
    b <- a[i, ]
    if (length(b$trees[[1]]) > 0) {

      if (country == "spain") {

        if (b$stand_type == "individual") {

            # list object to store results per species.
            df <- list()

            # Loop through species and individual trees.
            for (j in unique(b$trees[[1]]$species)) {
              y <- b$trees[[1]] |> dplyr::filter(species == j)
              factor_diam <- factor_diam_IFN(y$dbh)
              z <- sapply(1:nrow(y), function(k) kernsmooth(x[[j]], y$dbh[k], width = width) * factor_diam[k])
              df[[j]] <- apply(z, 1, sum)
            }

            # Store and change 'stand_type' to "ipm".
            b$trees[[1]] <- df
            b$stand_type <- "ipm"
        }
      }  else if (country == "usa") {
        stop("Calculations for country = 'usa' have not yet been implemented")
      } else if (country == "france") {
        stop("Calculations for country = 'france' have not yet been implemented")
      }
    }
    a[i, ] <- b
  }

  if (verbose) cat("\n")

  return(a)
}
