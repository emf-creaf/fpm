#' Calculates basal area per plot and species.
#'
#' @description
#' It calculates the basal area and number of trees, total and per species, for a plot.
#'
#' @param a a \code{sf} object containing a single row.
#' @param param param a named \code{list} of parameters (see \code{Details} below).
#' @param verbose
#'
#' @return
#' The input \code{sf} object with new fields added, namely \code{ba},
#' \code{ntrees}, \code{ba_species} and \code{ntrees_species}, corresponding
#' to the total basal area (m2) and total number of trees, plus basal area and
#' number of trees per species, respectively.
#'
#' @details
#' This function is used by \code{update_stands}. Inputs are not checked for correctness.
#'
calc_descriptive <- function(a, param = list()) {


  # Retrieve parameters.
  country <- param$country
  x <- param$integvars
  h <- param$h


  # Other parameters.
  ba <- ntrees <- data.frame()
  stand_type <- a$stand_type
  cnst <- pi/40000


  # Calculations.
  b <- a$trees[[1]]   # Shorter name.
  if (country == "spain") {
    if (length(b) > 0) {
      if (stand_type == "individual") {
        b$factor_diam <- factor_diam_IFN(b$dbh)

        y <- b |> dplyr::group_by(species) |> dplyr::summarise(ba = sum(factor_diam * dbh^2) * cnst)
        ba <- split(y$ba, y$species)

        y <- b |> dplyr::group_by(species) |> dplyr::summarise(ntrees = sum(factor_diam))
        ntrees <- split(y$ntrees, y$species)

      } else if (stand_type == "ipm") {
        ba <- sapply(names(b), function(j) quadrature(b[[j]] * x[[j]]^2, h[[j]]) * cnst, simplify = F)
        ntrees <- sapply(names(b), function(j) quadrature(b[[j]], h[[j]]), simplify = F)
if (sum(is.na(unlist(ntrees)))>0) browser()
      }

      a$ba_species[[1]] <- ba
      a$ba <- sum(unlist(ba))
      a$ntrees_species[[1]] <- ntrees
      a$ntrees <- sum(unlist(ntrees))

    }
  } else if (country == "usa") {
    stop("Calculations for country = 'usa' have not yet been implemented")
  } else if (country == "france") {
    stop("Calculations for country = 'france' have not yet been implemented")
  }


  return(a)
}
