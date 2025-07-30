#' Calculates structural parameters per plot and species.
#'
#' @description
#' It calculates (a) basal area (m2/ha), (b) number of trees and (c) sum of cube of tree dbh (m3/ha),
#' total and per species, for a plot.
#'
#' @param sf a \code{sf} object containing a single row.
#' @param param a named \code{list} of parameters (see \code{Details} below).
#' @param verbose
#'
#' @return
#' The input \code{sf} object with new fields added, namely \code{ba},
#' \code{ntrees} and \code{R3} (total values), and \code{ba_species}, \code{ntrees_species}
#' and \code{R3_species} (per species).
#'
#' @details
#' This function is used by several functions in the package. Inputs are not checked for correctness.
#'
calc_descriptive <- function(sf, param = list(), factor_diam_IFN = factor_diam_IFN) {


  # Retrieve parameters.
  country <- param$country
  x <- param$integvars
  h <- param$h


  # Other parameters.
  ba <- ba2 <- R3 <- ntrees <- data.frame()
  stand_type <- sf$stand_type


  # Calculations.
  b <- sf$trees[[1]]   # Shorter name.
  if (country == "spain") {
    if (length(b) > 0) {
      if (stand_type == "individual") {

        b <- b |> dplyr::mutate(factor_diam = factor_diam_IFN(b$dbh)) |> dplyr::group_by(species)

        # Basal area.
        y <- b |> dplyr::summarise(ba = sum(factor_diam * dbh^2))
        ba <- split(y$ba, y$species)

        # Number of trees.
        y <- b |> dplyr::summarise(ntrees = sum(factor_diam))
        ntrees <- split(y$ntrees, y$species)

        # Cube radius, as a proxy for trunk volume.
        y <- b |> dplyr::summarise(R3 = sum(factor_diam * dbh^3))
        R3 <- split(y$R3, y$species)

      } else if (stand_type == "ipm") {

        # Basal area.
        ba <- sapply(names(b), function(j) quadrature(b[[j]] * x[[j]]^2, h[[j]]), simplify = F)

        # Number of trees.
        ntrees <- sapply(names(b), function(j) quadrature(b[[j]], h[[j]]), simplify = F)

        # Cube dbh, as a proxy for trunk volume.
        R3 <- sapply(names(b), function(j) quadrature(b[[j]] * x[[j]]^3, h[[j]]), simplify = F)

      }


      # Multiply by appropriate constants to work in m2 per ha.
      ba <- lapply(ba, "*", pi/200^2)
      R3 <- lapply(R3, "*", 1/200^3)


      # Save values.
      sf$ba <- sum(unlist(ba))
      sf$ba2 <- sf$ba^2
      sf$R3 <- sum(unlist(R3))
      sf$ntrees <- sum(unlist(ntrees))
      sf$ba_species[[1]] <- ba
      sf$ntrees_species[[1]] <- ntrees

    }
  } else if (country == "usa") {
    stop("Calculations for country = 'usa' have not yet been implemented")
  } else if (country == "france") {
    stop("Calculations for country = 'france' have not yet been implemented")
  }


  return(sf)
}
