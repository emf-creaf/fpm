#' IPM numerical quadrature
#'
#' @description
#' \code{ipm_quadrature} is used to calculate the integral of the continuous
#' distribution of adult trees times survival and diameter growth.
#'
#' @param a a \code{sf} object containing a number of POINT geometry types.
#' @param idplot identifier of single POINT, representing a tree stand, to modify.
#' @param expected_growth data.frame containing, for eac species, the expected growth
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
#' @export
#'
ipm_quadrature <- function(a, idplot, expected_growth, variance_growth, survival_prob, min_dbh) {

  id <- match(idplot, a$idplot)
  if (is.na(id)) stop("Could not find 'idplot' in 'a'")
  if (length(id) != 1) stop("Only one 'idplot' can be modified at the time")
  if (a$stand_type[id] != "ipm") stop("'stand_type' must be 'ipm'")

  # Abscissas per species.
  x <- attr(a, "integvars")
  nx <- nrow(x)

  sd_growth <- sqrt(variance_growth)

  # IPM quadrature.
  # if (attr(a, "country") == "spain") {
  #
  #   ng <- length(N)
  #   g.matrix <- matrix(0,ng,ng)
  #   for (i in 1:ng) g.matrix[i,] <- c(rep(0,i-1),dlnorm(y[1:(ng-(i-1))]-min_dbh,meanlog=gr[i],sdlog=sd.va[i]))
  #   Nsu <- N*su
  #   N.new <- sapply(1:ng,function(i) MiscMath::quad_ext_simpson(Nsu*g.matrix[,i],h))
  # }
  # return(N.new)

  # if (idplot == "ID7") browser()

  # Select plot and species.
  b <- a[id, ]$trees[[1]]
  sp <- colnames(b)

  # Abscissae intervals for species present in the plot.
  h <- unlist(x[2, sp, drop = F] - x[1, sp, drop = F])

  for (i in sp) {

    # Expected growth and sd for species 'i'.
    g <- expected_growth[, i]
    s <- sd_growth[i]

    # Big matrix to store 2D growth term.
    gmat <- matrix(0, nx, nx)
    xx <- x[, i] - min_dbh[i]
    jseq <- 1:nx
    for (j in 1:nx) {
      gmat[j, jseq] <- dlnorm(xx, meanlog = g[j], sdlog = s[i])
      xx <- xx[-length(xx)]
      jseq <- jseq[-1]
    }

    # Former tree distribution times survival.
    Nsu <- b[, i, drop = F] * survival_prob[, i, drop = F]

    # Numerical quadrature.
    b[, i] <- sapply(1:nx, function(j) MiscMath::quad_ext_simpson(Nsu[, i]*gmat[, j],h[i]))
  }

  # Update trees.
  a[id, ]$trees[[1]] <- b

  return(a)

}
