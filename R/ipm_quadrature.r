#' IPM numerical quadrature
#'
#' @description
#' \code{ipm_quadrature} is used to calculate the integral of the continuous
#' distribution of adult trees times survival and diameter growth.
#'
#' @param a a \code{sf} object containing a number of POINT geometry types.
#' @param idplot identifier of single POINT, representing a tree stand, to modify.
#' @param expected_growth data.frame containing, for each species, the expected growth
#' in dbh over previous dbh.
#' @param variance_growth named vector with variance of logarithmic growth per species.
#' @param survival_prob probability of survival
#' @param min_dbh minimum dbh for adult tree class. It must be a named numeric vector
#' with one value per species.
#'
#' @examples
#' # First initialize one single stand.
#' a <- start_stand("ID1", 5, 45, "EPSG:4326")
#' a <- set_attributes(a)
#'
#' # Now we add tree information.
#' df <- data.frame(species = c(sample(c("Pnigra","Phalep"),5,replace=T)),
#' dbh1 = 7.5+runif(5)*20, factor_diam1 = sample(c(127.324, 31.83099),5,replace=T))
#' a <- build_stand(a, "ID1", df, "trees", "individual", 1990)
#'
#' # Convolve to obtain a continuous distribution.
#' x <- data.frame(Pnigra = seq(7.5,200,length=100), Phalep = seq(7.5,250,length=100))
#' a <- set_attributes(a, integvars = x)
#' a <- smooth_stand(a)
#'
#' # "Fake" IPM functions.
#' gr <- data.frame(Pnigra=rep(.1, nrow(x)), Phalep=rep(.15, nrow(x)))
#' va <- data.frame(Pnigra=2, Phalep=2.5)
#' su <- data.frame(Pnigra=rep(1, nrow(x)), Phalep=rep(1, nrow(x)))
#'
#' # Apply quadrature.
#' b <- ipm_quadrature(a, "ID1", gr, va, su, min_dbh = 7.5)
#'
#' @return
#' \code{sf} object with projected adult tree population.
#'
#'
ipm_quadrature <- function(a, idplot, expected_growth, variance_growth, survival_prob, quadrature = c("trapezoidal", "simpson")) {

  id <- if (is.null(idplot)) 1:length(a$idplot) else match(idplot, a$idplot)

  if (is.na(id)) stop("Could not find 'idplot' in 'a'")

  if (a$stand_type[id] != "ipm") stop("'stand_type' must be 'ipm'")

  if (class(expected_growth) != "data.frame") stop("Input 'expected_growth' must be a data.frame")
  if (class(variance_growth) != "data.frame") stop("Input 'variance_growth' must be a data.frame")
  if (ncol(expected_growth) != ncol(variance_growth))
    stop("Inputs 'expected_growth' and 'variance_growth' have different columns")
  if (nrow(expected_growth) != nrow(variance_growth))
    stop("Inputs 'expected_growth' and 'variance_growth' have different number of rows")

  if (a[id, ]$stand_type != "ipm") stop("Some plots are not 'ipm' type")

  # Abscissas per species.
  x <- attr(a, "integvars")

  # Check tree stands.

  for (i in id) {

    # Select plot.
    b <- data.frame(a[id, ]$trees[[1]])

    # If "trees" list is not empty.
    if (nrow(b) > 0) {

      # Check country.
      if (attr(b, "country") == "spain") {

        # Retrieve species.
        trees <- data.frame(b$trees[[1]])
        species <- unique(trees$species)
        nsp <- length(species)

    # if (!all(sp %in% colnames(variance_growth)))
    #   stop("Inputs 'expected_growth' and 'variance_growth' have different species")
    #
    # # Check min_dbh.
    # if (!all(sp %in% names(min_dbh))) stop("Species and integvars column names do not match")

        if (!all(sp %in% colnames(x))) stop(paste0("Species for plot ", b$idplot, " and integvars column names do not match"))

    # From variance to standard deviation.
        sd_growth <- sqrt(variance_growth)

    # Former tree distribution times survival per species.
        Nsu <- b[, sp, drop = F] * survival_prob[, sp, drop = F]

    # Abscissae intervals for species present in the plot.
        h <- unlist(x[2, sp, drop = F] - x[1, sp, drop = F])

        for (i in sp) {

          # Big matrix for growth term.
          gmat <- matrix(0, nx, nx)
          xx <- x[, i] - min_dbh[i]
          jseq <- 1:nx
          for (j in 1:nx) {
            gmat[j, jseq] <- dlnorm(xx, meanlog = expected_growth[j, i], sdlog = sd_growth[j,i])
            xx <- xx[-length(xx)]
            jseq <- jseq[-1]
          }

          # Numerical quadrature with trapezoidal rule.
          b[, i] <- numquad_vm(Nsu[, i], gmat, h[i], "simpson")

        }

    # Update trees.
    a[id, ]$trees[[1]] <- quad_growth()
      }
    }
  }

  return(a)

}
