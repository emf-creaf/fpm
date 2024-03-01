#' Start \code{sf} object to represent a tree stand
#'
#'#' @description
#' Create a zero-row \code{sf} object to contain stand data.
#'
#' @param param a named \code{list} of parameters (see \code{Details} below).
#' Default is "spain".
#'
#' @details This simple function creates a zero-rows, simple feature (a.k.a. \code{sf})
#' object to store all data for a single tree stand.
#' The \code{param} input can supply the following components:
#' * \code{country} string indicating which country the stand belongs to.
#' At this moment, "spain" (default), "france" or "usa" are valid inputs, though
#' only calculations for the Spanish Inventario Forestal Nacional have been
#' implemented. Lower or upper case letters can be used.
#' * \code{integvars} optional \code{list} where each named element contains the abscissas for
#' the species to be modeled.
#' * \code{min_dbh} named vector containing the minimum dbh after which a tree will
#' be considered as an adult individual.
#' * \code{max_dbh} named vector containing the maximum possible dbh for each trees species.
#' * \code{crs} coordinate reference system of stand. If missing, NA will be assumed.
#'
#' @return
#' A \code{sf} object with the following attributes (if provided in \code{param}):
#' \code{country}, \code{integvars}, \code{min_dbh}, \code{max_dbh}, \code{crs} and
#' \code{species}. New fields in the \code{sf} object are also created, depending
#' on which country has been selected.
#'
#' @export
#'
#' @examples
#' a <- start_stands()
#'
start_stands <- function(param = list(country = "spain")) {


  # Start sf object with dummy coordinates.
  a <- sf::st_as_sf(data.frame(X = 0, Y = 0), coords = c("X", "Y"))


  # At least parameter country must be set at the start.
  stopifnot("Input list 'param' cannot be empty" = length(param) > 0)
  stopifnot("Parameter 'country' in input list 'param' is mandatory" = !is.null(param[["country"]]))
  country <- param[["country"]]
  attr(a, "country") <- match.arg(country, choices = c("spain", "usa", "france"))


  # Set remaining parameters, but only if there is any (apart from "country").
  if (length(param) > 0) {
    ctrl <- within(param, rm(country))
    if (length(ctrl) > 0) a <- set_parameters(a, param = ctrl)
  }


  if (country == "spain") {

    # Empty identifier column.
    a$idplot <- ""

    # Stand type is character.
    a$stand_type <- ""


    # Empty date column.
    a$date <- ""


    # Empty lists that will contain seedlings, saplings and trees.
    a$seedlings <- a$saplings <- a$trees <- vector("list", 1)

  } else if (country == "usa") {
    stop("Calculations for country = 'usa' have not yet been implemented")

  } else if (country == "france") {
    stop("Calculations for country = 'france' have not yet been implemented")

  }


  return(a[-1, ])
}
