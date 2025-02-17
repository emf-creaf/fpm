#' Calculates structural parameters per plot and species.
#'
#' @description
#' It calculates the basal area, number of trees and sum of cube of tree radius,
#' total and per species, for a plot.
#'
#' @param a a \code{sf} object containing a single row.
#' @param param param a named \code{list} of parameters (see \code{Details} below).
#' @param verbose
#'
#' @return
#' The input \code{sf} object with new fields added, namely \code{ba},
#' \code{ntrees} and \code{R3} (total values), and \code{ba_species}, \code{ntrees_species}
#' and \code{R3_species} (per species).
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
  ba <- ntrees <- R3 <- data.frame()
  stand_type <- a$stand_type
  cnst <- pi/40000


  # Calculations.
  b <- a$trees[[1]]   # Shorter name.
  if (country == "spain") {
    if (length(b) > 0) {
      if (stand_type == "individual") {

        b <- b |> dplyr::mutate(factor_diam = factor_diam_IFN(b$dbh)) |> dplyr::group_by(species)

        # Basal area.
        y <- b |> dplyr::summarise(ba = sum(factor_diam * dbh^2) * cnst)
        ba <- split(y$ba, y$species)

        # Number of trees.
        y <- b |> dplyr::summarise(ntrees = sum(factor_diam))
        ntrees <- split(y$ntrees, y$species)

        # Cube radius, as a proxy for trunk volume.
        y <- b |> dplyr::summarise(R3 = sum(factor_diam * (dbh/200)^3))
        R3 <- split(y$R3, y$species)


      } else if (stand_type == "ipm") {

        # Basal area.
        ba <- sapply(names(b), function(j) quadrature(b[[j]] * x[[j]]^2, h[[j]]) * cnst, simplify = F)

        # Number of trees.
        ntrees <- sapply(names(b), function(j) quadrature(b[[j]], h[[j]]), simplify = F)

        # Cube radius, as a proxy for trunk volume.
        R3 <- sapply(names(b), function(j) quadrature(b[[j]] * (x[[j]]/200)^3, h[[j]]), simplify = F)

      }

      a$ba_species[[1]] <- ba
      a$ba <- sum(unlist(ba))
      a$ntrees_species[[1]] <- ntrees
      a$ntrees <- sum(unlist(ntrees))
      a$R3_species[[1]] <- R3
      a$R3 <- sum(unlist(R3))

    }
  } else if (country == "usa") {
    stop("Calculations for country = 'usa' have not yet been implemented")
  } else if (country == "france") {
    stop("Calculations for country = 'france' have not yet been implemented")
  }


  return(a)
}
