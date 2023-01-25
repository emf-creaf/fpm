#' Descriptive statistics per tree stand
#'
#' @description
#' Calculate descriptive statistics (number of trees, basal area) at tree-stand level per species
#'
#' @param a a \code{sf} object containing a number of POINT geometry types.
#' @param idplot identifiers of POINT elements representing tree-stands. If NULL,
#' all plots will be used.
#'
#' @return
#' A \code{sf} object with aggregate values (number of trees, basal area) per
#' tree stand and tree species.
#'
#' @details
#' When the tree-stand information is given as a continuous distribution, an
#' appropriate numerical quadrature is employed. In this case, the \emph{integvars}
#' attribute must be set in advance.
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
#' # Update descriptive statistics for continuous data.
#' a <- stand_descriptive(a)
#' b <- stand_descriptive(b)
#'
stand_descriptive <- function(a, idplot = NULL, quadrature = c("trapezoidal", "simpson"), progressbar = T) {

  # Checks.
  if (!is.null(idplot)) {
    id <- match(idplot, a$idplot)
    if (any(is.na(id))) stop(cat("Could not find some 'idplots' in input 'a'"))
  } else {
    id <- 1:length(a$idplot)
  }
  quadrature <- match.arg(quadrature)
  country <- match.arg(tolower(attr(a, "country")), c("spain"))

  # Shorter name for quadrature function.
  q <- function(y, h) if (quadrature == "trapezoidal") quad_trapez(y, h) else quad_ext_simpson(y, h)

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

  # Either sum trees or integrate continuous distribution.
  flag.ipm <- F
  for (i in id) {
    df <- a[i, ]$trees[[1]]

    # Progress bar.
    if (progressbar) pb$tick()

    # Calculate only if there are trees.
    if (length(df) > 0) {

      # If "individual", use dplyr.
      if (tolower(a[i, ]$stand_type) == "individual") {
        if (country == "spain") {
          df <- df %>% dplyr::group_by(species)
          a[i,]$species[[1]] <- (df %>% dplyr::distinct(species))$species
          a[i,]$BA_species[[1]] <- as.data.frame(df %>% dplyr::summarise(BA=(pi/200^2)*sum(factor_diam1*dbh1^2)))
          a[i,]$N_species[[1]] <- as.data.frame(df %>% dplyr::summarise(N=sum(factor_diam1)))
        } else if (country == "usa") {
        } else if (country == "france") {
        }
      }



      # To be implemented (matrix population models).
      if (tolower(a[i, ]$stand_type) == "mpm") {
      }



    # If "ipm", use numerical quadratures (since data sets are continuous).
      if (tolower(a[i, ]$stand_type) == "ipm") {

        # If any plot is "ipm", attribute "integvars" must be present in the 'sf'.
        # Then, retrieve abscissas only if needed, and only once. Grid spacing
        # is also calculated, as a vector, for each species.
        if (!flag.ipm) {
          if (any(tolower(a$stand_type) == "ipm")) {
            x <- attr(a, "integvars")
            if (is.null(x)) stop("Attribute 'integvars' has not been set")
            h <- unlist(x[2,]-x[1,])
          }
          flag.ipm <- TRUE
        }

        if (any(country == "spain")) {
          coln <- colnames(df)
          a[i,]$species[[1]] <- coln
          a[i,]$BA_species[[1]] <-
            data.frame(species = coln, BA = unname(sapply(coln, function(j) q(df[, j]*x[, j]^2, h[j]))*(pi/40000)))
          a[i,]$N_species[[1]] <-
            data.frame(species = coln, N = unname(sapply(coln, function(j) q(df[, j], h[j]))))
        } else if (country == "usa") {
        } else if (country == "france") {
        }
      }
      a[i,]$BA_stand <- sum(a[i,]$BA_species[[1]]$BA)
      a[i,]$N_stand <- sum(a[i,]$N_species[[1]]$N)

    } else {
      a[i,]$species[[1]] <- list()
      a[i,]$BA_species[[1]] <- list()
      a[i,]$N_species[[1]] <- list()
    }

  }

  return(a)
}
