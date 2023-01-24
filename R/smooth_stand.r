#' Convolution of discrete tree stands
#'
#' @description
#' Convolution of discrete tree stands per species with smoothing window
#'
#' @param a a \code{sf} object containing a number of POINT geometry types.
#' @param idplot identifiers of POINT elements representing tree stands to smooth.
#' @param smooth_type string indicating which smoothing window to use. Presently,
#' only \code{smooth_type = "gaussian"} option is available.
#' @param width width of smoothing window.
#' @param progressbar logical to print a progress bar with package \code{progress}.
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
#' dbh1 = 7.5+runif(5)*20, factor_diam1 = sample(c(127.324, 31.83099),5,replace=T))
#' a <- build_stand(a, paste0("ID",i), df, "trees", "individual", 1990)
#' }
#'
#' # Convolve to obtain a continuous distribution.
#' x <- data.frame(Pnigra = seq(7.5,200,length=1000), Phalep = seq(7.5,250,length=1000))
#' a <- set_attributes(a, integvars = x)
#' b <- smooth_stand(a)
#'
smooth_stand <- function(a, idplot, smooth_type = "gaussian", width = 2, progressbar = T) {

  mf <- match.call()
  m <- match(c("a", "idplot", "stand_type", "smooth_type", "width"), tolower(names(mf)[-1]))

  # Does 'idplot' exist?
  id <- if (is.na(m[2])) 1:length(a$idplot) else match(idplot, a$idplot)
  if (any(is.na(id))) stop("Could not find 'idplot' in 'a'")

  # We need the integration variable for the calculations.
  x <- attr(a, "integvars")
  if (is.null(x)) stop("Attribute 'integvars' is missing")
  colx <- colnames(x)
  nx <- nrow(x)

  # If progress is TRUE, print a progress bar.
  if (progressbar) {
    pb <- progress_bar$new(format = "(:spin) [:bar] :percent [Elapsed time: :elapsedfull || Estimated time remaining: :eta]",
                           total = length(id),
                           complete = "=",   # Completion bar character
                           incomplete = "-", # Incomplete bar character
                           current = ">",    # Current bar character
                           clear = FALSE,    # If TRUE, clears the bar when finish
                           width = 100)
  }

  # Loop along all plots.
  for (i in id) {
    b <- a[i, ]

    # Progress bar.
    if (progressbar) pb$tick()

    # Smooth discrete data, but only if stand_type has been defined and set to "individual".
    if (!is.na(b$stand_type)) {
      if (b$stand_type == "individual") {

        # If "trees" list is not empty.
        if (length(b$trees[[1]]) > 0) {

          # Check country.
          if (attr(b, "country") == "spain") {

            # Species to smooth.
            trees <- data.frame(b$trees[[1]])
            species <- unique(trees$species)
            nsp <- length(species)

            # Check that all species are in 'integvars' data.frame.
            if (any(!(species %in% colx))) stop(cat("Species in stand ",i," do not match those in attribute 'integvars'\n"))

            # Big data.frame to store results per species column-wise.
            df <- data.frame(matrix(0,nx, nsp))
            colnames(df) <- species

            # Loop through species and individual trees.
            for (j in species) {
              xj <- x[, j]
              y <- trees[trees$species == j, , drop = F]
              z <- numeric(nx)
              for (k in 1:nrow(y)) z <- z + kernsmooth(xj, y$dbh1[k] , width = width) * y$factor_diam1[k]
              df[, j] <- z
            }

            # Store and change 'stand_type' to "ipm".
            a[i, ]$trees[[1]] <- df
            a[i, ]$stand_type <- "ipm"
          }
        }
      }
    }
  }

  return(a)
}
