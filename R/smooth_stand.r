#' Title
#'
#' @param a
#' @param idplot
#' @param type
#' @param width
#'
#' @return
#' @export
#'
#' @examples
#' a <- start_inventory(letters[1:5],runif(5),runif(5),rep("individual",5),runif(5),"EPSG:4326")
#' a <- set_attributes(a)
#' df <- data.frame(species = c(sample(c("Pnigra","Phalep"),5,replace=T)),dbh = 7.5+runif(5)*20, factor_diam = sample(c(127.324, 31.83099),5,replace=T))
#' b <- add_data_stand(a, "c", df, "trees")
#' x <- cbind(seq(7.5, 200, length=2000)
#' bsmooth <- smooth_stand(b, "c")
#'
smooth_stand <- function(a, idplot, stand_type = "ipm", smooth_type = "gaussian", width = 2) {

  # Some plots are just not there.
  id <- match(idplot, a$idplot)
  if (any(is.na(id))) stop(cat("Could not find some 'idplots' in input 'a'"))

  # If stands are not of "individual" type, smoothing cannot be performed.
  if (any(!(a[id, ]$stand_type %in% "individual"))) stop("Some stands are not of 'individual' type")

  # From discrete to matrix or ipm-type stands.
  if (!any(stand_type %in% c("mpm", "ipm"))) stop("Input 'stand_type' must be 'mpm' or 'ipm'")

  # We need the integration variable for the calculations.
  x <- attr(a, "integration_variable")
  if (is.null(x)) stop("Attribute 'integration_variable' is missing")
  nx <- nrow(x)

  # Loop along all plots.
  for (i in id) {

    # Species to smooth.
    trees <- a[i, ]$trees[[1]]
    species <- unique(trees$species)
    nsp <- length(species)

    # Check that all species are in 'integration_variable' data.frame.
    colx <- colnames(x)
    if (any(!(species %in% colx))) stop(cat("Species in stand ",i," do not match those in 'integration_variable' attribute\n"))

    # Big matrix to store results per species column-wise.
    df <- matrix(0,nx, nsp, dimnames = list(c(), species))

    if (stand_type == "ipm") {
      for (j in 1:nsp) {
        y <- trees[trees$species == species[j], , drop = F]
        for (k in 1:nrow(y)) {
          df[, species[j]] <- df[, species[j]] +
            MiscStat::fast_kernsmooth(x[, species[j]], y$dbh1[k] , width = width) * y$factor_diam1[k]
        }
      }
    } else if (stand_type == "mpm") {
    }

    # Store and change 'stand_type' to "ipm".
    a[i, ]$trees[[1]] <- as.data.frame(df)
  }

  # Finally, stand_type is set.
  a[id, ]$stand_type <- "ipm"

  return(a)
}
