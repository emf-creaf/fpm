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

  if (!any(class(a) %in% "inventory")) stop("Object 'a' must be of 'inventory' class")

  if (length(idplot) > 1) stop("Input 'idplot' must have length = 1")
  id <- match(idplot, a$idplot)
  if (is.na(id)) stop(cat("Stand", idplot, "does not exist\n"))
  if (a[id, ]$stand_type != "individual") stop("Stand is not of 'individual' type")
  x <- attr(a, "integration_variable")
  if (is.null(x)) stop("Attribute 'integration_variable' is missing")
  if (!any(stand_type %in% c("mpm", "ipm"))) stop("Input 'stand types must be equal to 'mpm' or 'ipm'")

  # Species to smooth.
  trees <- a[id, ]$trees[[1]]
  species <- unique(trees$species)
  nsp <- length(species)

  # Check that they are in 'integration_variable'.
  colx <- colnames(x)
  if (any(!(species %in% colx))) stop("Species in stand do not match those in 'integration_variable' attribute")

  # Big matrix to store results per species column-wise.
  nx <- ifelse(stand_type %in% c("mpm","ipm"), nrow(x), length(x))
  df <- matrix(0,nx, nsp)
  colnames(df) <- species

  if (stand_type == "ipm") {
    xx <- subset(x, select = species)
    for (i in 1:nsp) {
      y <- subset(trees, species == species[i])
      for (j in 1:nrow(y)) {
        df[, i] <- df[, i] + MiscStat::fast_kernsmooth(xx[, i], y$dbh1[j] , width = width) * y$factor_diam1[j]
      }
    }
  } else if (stand_type == "mpm") {
  }

  # Store and change 'stand_type' to "ipm".
  a[id, ]$trees[[1]] <- as.data.frame(df)
  a[id, ]$stand_type <- "ipm"

  return(a)
}
