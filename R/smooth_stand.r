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
#' x <- seq(7.5, 200, length=2000)
#' bsmooth <- smooth_stand(b, "c", x)
#'
smooth_stand <- function(a, idplot, x, stand_type = "ipm", smooth_type = "gaussian", width = 2) {

  if (length(idplot) > 1) stop("Input 'idplot' must have length = 1")
  id <- match(idplot, a$idplot)
  if (is.na(id)) stop(cat("Stand", idplot, "does not exist\n"))
  if (a[id, ]$stand_type != "individual") stop("Stand is not of 'individual' type")
  if (!any(stand_type %in% c("mpm", "ipm"))) stop("Stand type must be 'mpm' or 'ipm'")
  if (stand_type == "mpm") {
    if (!is.data.frame(x)) {
      stop("If 'stand_type' is equal to 'mpm', input 'x' must be a two-column data.frame")
      nx <- nrow(x)
    } else {
      if (!is.vector(x)) stop("If 'stand_type' is equal to 'ipm', input 'x' must be a vector")
      nx <- length(x)
    }
  }

  # Species to smooth.
  trees <- a[id, ]$trees[[1]]
  species <- unique(trees$species)
  nsp <- length(species)

  # Big matrix to store results per species column-wise.
  nx <- ifelse(stand_type == "mpm", nrow(x), length(x))
  df <- matrix(0,nx, nsp)
  colnames(df) <- species

  if (stand_type == "ipm") {
    for (i in 1:nsp) {
      y <- subset(trees, species == species[i])
      for (j in 1:nrow(y)) {
        df[, i] <- df[, i] + MiscStat::fast_kernsmooth(x, y$dbh[j] , width = width) * y$factor_diam[j]
      }
    }
  } else if (stand_type == "mpm") {

  }

  # Store and change 'stand_type' to "ipm".
  a[id, ]$trees[[1]] <- as.data.frame(df)
  a[id, ]$stand_type <- "ipm"

  return(a)
}
