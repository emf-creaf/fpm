#' Start \code{sf} object to represent a tree stand
#'
#'#' @description
#' Create a zero-row \code{sf} object to contain tree stand data.
#'
#' @param param_data a named \code{list} of parameters (see \code{Details} below).
#'
#' @details This simple function creates a zero-rows, simple feature (a.k.a. \code{sf})
#' object to store all data for a single tree stand.
#' The \code{param} input can supply the following components:
#' * \code{integvars} optional \code{list} where each named element contains the abscissas for
#' the species to be modeled.
#' * \code{mindbh} named vector containing the minimum dbh after which a tree will
#' be considered as an adult individual.
#' * \code{maxdbh} named vector containing the maximum possible dbh for each trees species.
#' * \code{crs} coordinate reference system of stand. If missing, NA will be assumed.
#'
#' @return
#' A \code{sf} object with the following attributes (if provided in \code{param}):
#' \code{integvars}, \code{mindbh}, \code{maxdbh}, \code{crs} and
#' \code{species}.
#'
#' @export
#'
#' @examples
#'  df <- data.frame(idplot = as.character(1:2),
#'  x = 3:4, y = 5:6,
#'  trees = I(list(data.frame(dbh = 1:2, species = letters[1:2]),
#'  data.frame(dbh = 3:4, species = letters[1:2]))))
#' sf <- start_stands(df)
#'
start_stands <- function(tree_data = NULL, param = NULL, verbose = TRUE) {


  if (verbose) {
    fn_name <- as.character(sys.call()[[1]])
    cat("\n")
  }


  # Check input data.frame, if not NULL.
    check_input_tree_data(tree_data)


  if (verbose) {
    if (is.null(param)) {
      cli::cli_alert_warning("[{fn_name}] Input list 'param' is empty")
    } else {
      if (!is.list(param)) {
        cli::cli_abort("Input 'param' must be a list object")
      }
    }
  }


  # Start by creating an empty POINT sf object.
  sf_plots <- sf::st_sf(
    id = integer(),
    name = character(),
    geometry = sf::st_sfc(sf::st_point())[0]
  )


  # Empty identifier column.
  sf_plots$idplot <- character()


  # Stand type is character.
  sf_plots$stand_type <- character()


  # Empty date column.
  sf_plots$date <- numeric()


  # Empty lists that will contain seedlings, saplings and trees.
  sf_plots$seedlings <- sf_plots$saplings <- sf_plots$trees <- list()


  # Continue here.
  if (!is.data.frame(tree_data)) {

  }


  # Start sf object with dummy coordinates.
  sf <- sf::st_as_sf(data.frame(X = 0, Y = 0), coords = c("X", "Y"))


  # At least parameter country must be set at the start.
  stopifnot("Input list 'param' cannot be empty" = length(param) > 0)


  # Set remaining parameters, but only if there is any (apart from "country").
  ctrl <- within(param, rm(country))
  if (length(ctrl) > 0) sf <- set_parameters(sf, param = ctrl)








  return(sf[-1, ])
}
