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
ipm_spain <- function(a, dat, reg_growth, reg_variance, reg_survival, reg_ingrowth, reg_saplings,
                      quadrature = "simpson", progressbar = T) {

  id <- match(a$idplot %in% dat$idplot)
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
  mindbh <- attr(a, "min_dbh")
  maxdbh <- attr(a, "max_dbh")

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

        df <- data.frame(a$trees[[i]], check.names = F)    # Shorter than writing a$trees[[i]].
        species <- colnames(df)
        nsp <- ncol(df)

        # if (!all(sp %in% colnames(variance_growth)))
        #   stop("Inputs 'expected_growth' and 'variance_growth' have different species")
        #
        # # Check min_dbh.
        # if (!all(sp %in% names(min_dbh))) stop("Species and integvars column names do not match")

        # if (!all(sp %in% colnames(x))) stop(paste0("Species for plot ", b$idplot, " and integvars column names do not match"))

        if (!any(is.na(dat[i, ]))) {

          for (ispecies in species) {

      browser()

            # Former tree distribution times survival per species.
            Nsu <- df[, ispecies, drop = F] * predict(reg_survival[[ispecies]], newdata = dat)

            # Growth term.
            growth <- predict(reg_growth, newdata = dat)

            # Term for standard deviation of growth term.
            sd_growth <- sqrt(predict(reg_variance[[ispecies]], newdata = dat, type = "response"))

            # Big matrix for growth term.
            gmat <- matrix(0, nx, nx)
            xx <- x[, ispecies] - mindbh[ispecies]
            iseq <- 1:nx
            for (ix in 1:nx) {
              gmat[j, jseq] <- dlnorm(xx, meanlog = growth[ix], sdlog = sd_growth[ix])
              xx <- xx[-length(xx)]
              iseq <- iseq[-1]
            }

            # Numerical quadrature with trapezoidal rule.
            df[, ispecies] <- numquad_vm(Nsu, gmat, h[ispecies], quadrature)
          }
          # Update trees.
          a$trees[[i]] <- df
        }

      }


      ########################################## Saplings.



    }
  }
  cat("\n")

  return(a)

}
