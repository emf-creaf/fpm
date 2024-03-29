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
                      lambda_ingrowth, reg_saplings, quadrature = "simpson", progressbar = T, system_time = T) {

  # Info about total computing time.
  if (system_time) time1 <- as.numeric(Sys.time())

  # Check idplot
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

  # Abscissas per species and intervals.
  x <- attr(a, "integvars")
  h <- attr(a, "h")
  nx <- nrow(x)

  # If progress is TRUE, print a progress bar.
  if (progressbar) {
    pb <- txtProgressBar(min = 0,
                         max = length(id),
                         style = 3,
                         width = 50,
                         char = "=")
    cat("\n-> ipm_spain: Calculating model...\n")
  }

  # Main loop.
  icount <- 1
  for (i in id) {

    # Progress bar.
    if (progressbar) setTxtProgressBar(pb, icount)
    icount <- icount + 1

    ########################################## Adult trees.

    # Flags are useful below.
    species_trees <- NULL
    species_sapl <- NULL
    flag_saplings <- length(a$saplings[[i]]) > 0
    flag_trees <- !is.na(a$stand_type[i]) & (a$stand_type[[i]] == "ipm")
    flag_ba <- length(a$basal_area_species[[i]]) > 0

    # There can be no NA among predictors.
    if (!any(is.na(dat[i, ]))) {

      # Continue if stand_type is "ipm".
      if (flag_trees) {
        if (!any(is.na(dat[i, ]))) {

          trees <- data.frame(a$trees[[i]], check.names = F)    # Shorter than writing a$trees[[i]] very time.
          species_trees <- colnames(trees)

          # data.frame for predictions.
          newdata <- as.data.frame(lapply(dat[i, ], rep, nx))

          for (j in species_trees) {

            # Abscissas for ispecies.
            newdata$dbh <- x[, j]
            newdata$max_dbh <- max_dbh[j]

            # Former tree distribution times survival per species.
            Nsu <- trees[, j] *
              predict(reg_survival[[j]], newdata = newdata, type = "response")

            # Growth term.
            growth <- predict(reg_growth[[j]], newdata = newdata, type = "response")

            # Term for standard deviation of growth term.
            sd_growth <- sqrt(predict(reg_variance[[j]], newdata = newdata, type = "response"))

            # Big matrix for growth term.
            gmat <- matrix(0, nx, nx)
            xx <- x[, j] - min_dbh[j]
            kseq <- 1:nx
            for (k in 1:nx) {
              gmat[k, kseq] <- dlnorm(xx, meanlog = growth[k], sdlog = sd_growth[k])
              if (sum(is.na(dlnorm(xx, meanlog = growth[k], sdlog = sd_growth[k])))>0) browser()
              xx <- xx[-length(xx)]
              kseq <- kseq[-1]
            }

            # Numerical quadrature with trapezoidal rule.
            trees[, j] <- numquad_vm(Nsu, gmat, h[j], quadrature)
          }

        }
      }

      ########################################## Ingrowth.

      # Model predictions for new trees.
      if (flag_saplings) {

        # From long to wide.
        saplings <- a$saplings[[i]] %>% select(species, N) %>% tidyr::spread(species, N)
        species_sapl <- colnames(saplings)
        newtrees <- data.frame(matrix(0, nx, length(species_sapl)))
        colnames(newtrees) <- species_sapl

        # Number of saplings as a function of dbh. Internally ingrowth_model converts to individuals per ha.
        for (j in species_sapl) {
          newdata <- cbind(dat[i, ], saplings = saplings[, j])
          newtrees[, j] <- ingrowth_model(reg_ingrowth[[j]], newdata, x[, j], lambda_ingrowth[j], min_dbh[j])
        }
      }

      if (!flag_trees & !flag_saplings) stop("Something is wrong. There are no trees or saplings")

      # We know that there are either trees or saplings in the plot. If there are
      # no trees, we create a data.frame for the new trees.
      if (!flag_trees) {
        species_trees <- species_sapl
        trees <- data.frame(matrix(0, nx, length(species_trees)))
        colnames(trees) <- species_trees
      }

      # We add new trees to the pool of adult trees that just grew, if any.
      # There are two parts: one for the sapling species that are also present
      # as adult trees, and another for those that are not.
      if (flag_saplings) {
        a$stand_type[i] <- "ipm"
        k <- match(species_sapl, species_trees)
        names(k) <- species_sapl
        for (j in species_sapl) {
          if (is.na(k[j])) {
            trees <- cbind(trees, newtrees[, j])
            colnames(trees)[ncol(trees)] <- j
          } else {
            if (a$basal_area[[i]] < 15) {
              trees[, j] <- trees[, j] + newtrees[, j]
            }
          }
        }
      }

      a$trees[[i]] <- trees

      ########################################## Saplings.
      # There will be new saplings only if there were saplings
      # before or the basal area for a species is >0.

      if (flag_saplings | flag_ba) {
        species_all <- unique(c(species_sapl, names(a$basal_area_species[[i]])))
        new_sapl <- setNames(numeric(length(species_all)), species_all)
        for (j in species_all) {
          newdata <- cbind(dat[i, ],
                           saplings = ifelse(!flag_saplings, 0, find2(j, saplings)),
                           basal_area_species = ifelse(!flag_ba, 0, find2(j, a$basal_area_species[[i]])))
          if (newdata$saplings > 0 | newdata$basal_area_species > 0) new_sapl[j] <-
              saplings_model(reg_saplings[[j]], newdata = newdata)
        }

        # Back to long.
        a$saplings[[i]] <- data.frame(species=names(new_sapl), N = unname(new_sapl))
      }
    }

  }
  cat("\n\n")

  if (system_time) {
    time2 <- as.numeric(Sys.time())
    cat(paste0("\n ipm_spain: Total computing time = ",(time2-time1)/60, " minutes\n\n"))
  }

  return(a)
}
