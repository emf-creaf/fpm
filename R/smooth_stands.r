#' Convolution of discrete tree stands
#'
#' @description
#' Convolution of discrete tree stands per species with a smoothing window.
#'
#' @param a a \code{sf} object containing a number of POINT geometry types.
#' @param smooth_type string indicating which smoothing window to use. Presently,
#' only \code{smooth_type = "gaussian"} option is available.
#' @param verbose logical, if set to TRUE a progress bar will be printed on screen.
#' @param width width of smoothing window.
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
#' # First initialize one single stand.
#' a <- start_stand("ID1", 5, 45, "EPSG:4326")
#' a <- set_attributes(a)
#'
#' # Next, we merge other stands.
#' for (i in 2:10) {
#' b <- start_stand(paste0("ID",i), 5, 45, "EPSG:4326")
#' b <- set_attributes(b)
#' a <- merge_stands(a,b)
#' }
#'
#' # Now we add tree information.
#' for (i in 1:10) {
#' df <- data.frame(species = c(sample(c("Pnigra","Phalep"),5,replace=T)),
#' dbh = 7.5+runif(5)*20, factor_diam = sample(c(127.324, 31.83099),5,replace=T))
#' a <- build_stand(a, paste0("ID",i), df, "trees", "individual", 1990)
#' }
#'
#' # Convolve to obtain a continuous distribution.
#' x <- data.frame(Pnigra = seq(7.5,200,length=1000), Phalep = seq(7.5,250,length=1000))
#' a <- set_attributes(a, integvars = x)
#' b <- smooth_stand(a)
#'
smooth_stands <- function(a, smooth_type = "gaussian", width = 2, verbose = T) {

  mf <- match.call()
  m <- match(c("a", "idplot", "smooth_type", "width", "verbose"), tolower(names(mf)[-1]))


  # Does 'idplot' exist?
  id <- if (is.na(m[2])) 1:length(a$idplot) else match(idplot, a$idplot)
  stopifnot("Could not find some 'idplot' in 'a'" = all(!is.na(id)))


  # We need the integration variable for the calculations if any stand is "ipm".
  x <- get_parameters(a, "integvars")
  h <- get_parameters(a, "h")
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
  for (i in id) {

    # Progress bar.
    icount <- icount + 1
    if (verbose) setTxtProgressBar(pb, icount)


    # Calculate only if there are trees.
    b <- a[i, ]
    if (length(b) > 0) {

      if (b$stand_type == "individual") {

        if (country == "spain") {

            # list object to store results per species.
            df <- list()

            # Loop through species and individual trees.
            for (j in unique(b$trees[[1]]$species)) {
              y <- b$trees[[1]] |> dplyr::filter(species == j)
              z <- sapply(1:nrow(y), function(k) kernsmooth(x[[j]], y$dbh[k], width = width) * y$factor_diam[k])
              df[[j]] <- apply(z, 1, sum)
            }

            # Store and change 'stand_type' to "ipm".
            b$trees[[1]] <- df
            b$stand_type <- "ipm"
        }
      }
    }
    a[i, ] <- b
  }

  if (verbose) cat("\n")

  return(a)
}
