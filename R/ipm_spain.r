#' IPM model for Spain
#'
#' @description
#' \code{ipm_quadrature} is used to calculate the integral of the continuous
#' distribution of adult trees times survival and diameter growth.
#'
#' @param a a \code{sf} object containing a number of POINT geometry types.
#' @param idplot identifier of single POINT, representing a tree stand, to modify.
#' @param reg_growth
#' @param reg_variance
#' @param reg_survival
#' @param reg_ingrowth
#' @param reg_saplings
#' @param quadrature
#' @param progressbar

#'
#' @param a
#' @param reg_growth
#' @param reg_variance
#' @param reg_survival
#' @param reg_ingrowth
#' @param reg_saplings
#' @param quadrature
#' @param progressbar
#' @param dat
#' @param lambda_ingrowth
#'
#' @examples
#' # First initialize one single stand.
#' a <- start_stand("ID1", 5, 45, "EPSG:4326")
#' a <- set_attributes(a)
#'
#' # Now we add tree information.
#' df <- data.frame(species = c(sample(c("Pnigra","Phalep"),5,replace=T)),
#' dbh = 7.5+runif(5)*20, factor_diam = sample(c(127.324, 31.83099),5,replace=T))
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
#' @export
#'
ipm_spain <- function(a, dat, reg_growth, reg_variance, reg_survival, reg_ingrowth,
                      lambda_ingrowth, reg_saplings, quadrature = "simpson", progressbar = T) {

  id <- a$idplot %in% dat$idplot
  if (any(is.na(id))) stop("Inputs 'a' and 'dat' do not match")

  # Indices.
  id <- 1:nrow(a)

  stopifnot(tolower(attr(a, "country")) == "spain")

  quadrature <- match.arg(quadrature, c("trapezoidal", "simpson"))

  # if (class(expected_growth) != "data.frame") stop("Input 'expected_growth' must be a data.frame")
  # if (class(variance_growth) != "data.frame") stop("Input 'variance_growth' must be a data.frame")
  # if (ncol(expected_growth) != ncol(variance_growth))
  #   stop("Inputs 'expected_growth' and 'variance_growth' have different columns")
  # if (nrow(expected_growth) != nrow(variance_growth))

  # Minimum and maximum dbh per species.
  min_dbh <- attr(a, "min_dbh")
  max_dbh <- attr(a, "max_dbh")

  # Abscissas per species.
  x <- attr(a, "integvars")
  nx <- nrow(x)

  # Abscissae intervals for species present in the plot.
  h <- x[2, ] - x[1, ]

  # If progress is TRUE, print a progress bar.
  if (progressbar) {
    pb <- txtProgressBar(min = 1,
                         max = length(id),
                         style = 3,
                         width = 50,
                         char = "=")
    cat("\n-> ipm_spain: Calculating descriptive statistics...\n")
  }

  # Main loop.
  icount <- 1
  for (i in id) {

    # Progress bar.
    if (progressbar) setTxtProgressBar(pb, icount)
    icount <- icount + 1

    ########################################## Adult trees.

    if (!is.na(a$stand_type[i])) {

      # Continue if stand_type is "ipm".
      if (a$stand_type[[i]] == "ipm") {

        if (!any(is.na(dat[i, ]))) {

          trees <- data.frame(a$trees[[i]], check.names = F)    # Shorter than writing a$trees[[i]].
          species <- colnames(trees)
          nsp <- ncol(trees)

          # data.frame for predictions.
          newdata <- as.data.frame(lapply(dat[i, ], rep, nx))

          for (ispecies in species) {

            # Abscissas for ispecies.
            newdata$dbh <- x[, ispecies]
            newdata$max_dbh <- max_dbh[ispecies]

            # Former tree distribution times survival per species.
            Nsu <- trees[, ispecies] *
              predict(reg_survival[[ispecies]], newdata = newdata, type = "response")

            # Growth term.
            growth <- predict(reg_growth[[ispecies]], newdata = newdata, type = "response")

            # Term for standard deviation of growth term.
            sd_growth <- sqrt(predict(reg_variance[[ispecies]], newdata = dat, type = "response"))

            # Big matrix for growth term.
            gmat <- matrix(0, nx, nx)
            xx <- x[, ispecies] - min_dbh[ispecies]
            jseq <- 1:nx
            for (j in 1:nx) {
              gmat[j, jseq] <- dlnorm(xx, meanlog = growth[j], sdlog = sd_growth[j])
              xx <- xx[-length(xx)]
              jseq <- jseq[-1]
            }
            # Numerical quadrature with trapezoidal rule.
            trees[, ispecies] <- numquad_vm(Nsu, gmat, h[ispecies], quadrature)
          }
          # Update trees.
          a$trees[[i]] <- trees
        }

      }
    }


    ########################################## Ingrowth trees.




    ########################################## Saplings.

    saplings <- data.frame(a$saplings[[i]], check.names = F)
    if (length(saplings) > 0) {
      n <- nrow(saplings)
      newsaplings <- data.frame(species = saplings$species, N = numeric(n))
      for (j in 1:n) {
        newdata <- cbind(dat[i, ], saplings = saplings$N[j])
        newsaplings$N[j] <- predict(reg_ingrowth[[species[j]]], newdata = newdata, type = "response")
      }
      a$saplings[[i]] <- newsaplings
    }
  }
  cat("\n\n")

  return(a)
}
