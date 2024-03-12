#' Build tree-stand \code{sf} object
#'
#' @description
#' Add or modify fields in tree-stand \code{sf} object.
#'
#' @param a a \code{sf} object containing a number of POINT geometry types.
#' @param idplot single identifier representing a tree stand. If it is a new plot,
#' a warning will be issued (unless \code{verbose=FALSE}) and a single point
#' will be added to \code{a}.
#' @param data a \code{list} with named elements. See "Details" below.
#' @param verbose logical, if set to TRUE warning messages may be printed.
#'
#' @return
#' A \code{sf} object with added or modified \code{idplot} and/or fields.
#'
#' @details
#' Function \code{build_stand} must be called individually for
#' each tree stand.
#' When \code{stand_type = "individual"} and the attribute \code{country = "spain"},
#' the \code{df} \code{data.frame} for
#' trees (that is, when \code{data_type = "trees"}) must have two columns,
#' labeled "species" and "dbh". When it is seedlings or saplings that we want
#' to add to \code{a}, its format must be long, i.e. the input \code{data.frame} must have two columns, labeled
#' "species" and "n".
#'
#' @export
#'
#' @examples
#'
#' # First initialize stands.
#' a <- start_stands()
#' max_dbh <- list('Pinus halepensis' = 200, 'Pinus nigra' = 230)
#' a <- set_parameters(a, param = list(max_dbh = max_dbh, crs =  "EPSG:4326"))
#'
#' # Next, we add one stand.
#' df <- data.frame(species = c('Pinus halepensis', 'Quercus ilex'), dbh = c(8.6, 12.7))
#' a <- build_stand(a, "id1", data = list(df = df), verbose = T)
#'
build_stands <- function(a, idplot = NULL, data = list(), verbose = T) {

  # Check that input 'a' is an 'sf' object.
  stopifnot("Input 'a' must be an sf object" = inherits(a, "sf"))


  # Check idplot. It cannot be empty.
  stopifnot("Input 'idplot' is missing" = !is.null(idplot))
  stopifnot("Input 'idplot' must have length = 1" = (length(idplot) == 1))


  # Check 'data' input. It must be a list and must have length < 0.
  stopifnot("Input 'data' must be a list" = inherits(data, "list"))
  if (length(data) == 0) {
    warning("Empty 'data' input list. Returning 'a'")
    return(a)
  }


  # Retrieve parameters.
  p <- a |> get_parameters("country")
  country <- p$country
  x <- data$x
  y <- data$y
  data_type <- data$data_type
  stand_type <- data$stand_type
  df <- data$df
  date <- data$date


  # Check that 'country' is ok, if set.
  if (!is.null(country)) match.arg(country, choices = c("spain", "usa", "france"))


  # If idplot does not yet exist in 'a', add a new empty row at the bottom.
  # Each new empty row will have a POINT geometry.
  id <- match(idplot, a$idplot)
  if (is.na(id)) {
    if (verbose) warning("Adding a new 'idplot' to 'a'")
    b <- a[1, ]
    b$idplot <- idplot
    if (country == "spain") {
      b$date <- b$stand_type <- ""
      b$trees <- b$saplings <- b$seedlings <- vector("list", 1)
    }
    a[nrow(a) + 1,] <- b
    id <- nrow(a)
    a <- sf::st_cast(a, "POINT")
  }


  # If coordinates are given, set them.
  if (all(!is.null(x), !is.null(y))) {
    stopifnot("Each coordinate must have length = 1 (i.e. it is a single location)" = all(length(x) == 1, length(y) == 1))
    stopifnot("Coordinates must be real numbers" = all(is.numeric(x), is.numeric(y)))
    sf::st_geometry(a) <- sf::st_sfc(
        ifelse(a$idplot == idplot, sf::st_sfc(sf::st_point(c(x, y))), a$geometry),
        crs = sf::st_crs(a$geometry))
  }


  # Check date.
  if (!is.null(date)) {
    stopifnot("Input 'date' must be an object of class 'Date'" = inherits(date, "Date"))
    a$date[[id]] <- date
  }


  # Check that df is not empty or NULL.
  if (nrow(df) == 0 | is.null(df)) {
    if (verbose) warning("Input 'df' in list 'data' is empty or NULL. Returning...")
    return(a)
  }


  # Internal functions to check lower or upper bounds in "assert" call.
  is_larger <- function(bound, include.bound = F) {
    the_call <- deparse(sys.call())
    fun <- function(x) {
      if (is.null(x)) stop("bounds must be checked on non-null element")
      if (!is.numeric(x)) stop("bounds must only be checked on numerics")
      operator <- if (!include.bound) `>` else `>=`
      return(operator(x, bound) & !is.na(x))
    }

    attr(fun, "assertr_vectorized") <- TRUE
    attr(fun, "call") <- the_call
    return(fun)
  }

  is_smaller <- function(bound, include.bound = F) {
    the_call <- deparse(sys.call())
    fun <- function(x) {
      if (is.null(x)) stop("bounds must be checked on non-null element")
      if (!is.numeric(x)) stop("bounds must only be checked on numerics")
      operator <- if (!include.bound) `<` else `<=`
      return(operator(x, bound) & !is.na(x))
    }

    attr(fun, "assertr_vectorized") <- TRUE
    attr(fun, "call") <- the_call
    return(fun)
  }


  # Calculations for spain.
  if (country == "spain") {

    # For spain minimum dbh is 7.5cm.
    min_dbh <- 7.5

    # There is data to add to 'a'.
    if (!is.null(df)) {

      # If 'df' is provided, it must be a data.frame or a list.
      stopifnot("'df' must be a data.frame or a list" = any(inherits(df, "data.frame") | inherits(df, "list")))

      # Check data_type.
      data_type <- match.arg(data_type, c("trees", "seedlings", "saplings"))


      # Check stand_type when data_type = "trees".
      if (data_type == "trees") {
        stand_type <- match.arg(stand_type, c("individual", "ipm"))

  # Checks that are carried out below:
  # - There are no NA values, neither in trees nor in seedlings/saplings.
  # - Tree dbh is always > 0.
  # - Number of seedlings/saplings is always > 0.
  # - Seedlings/saplings are not duplicated.

        if (stand_type == "individual") {
          stopifnot("Input 'df' must be a data.frame when 'stand_type' = 'individual'" = inherits(df, "data.frame"))
          df <- df |>
            assertr::verify(assertr::has_only_names("species", "dbh")) |>
            assertr::assert_rows(assertr::num_row_NAs, function(x) x == 0, dbh) |>
            assertr::verify(dbh > min_dbh)

        } else {
          stopifnot("Input 'df' must be a list when 'stand_type' = 'ipm'" = inherits(df, "list"))
          df <- df |> assertr::assert(is_larger(min_dbh), dplyr::everything()) |>
            assertr::assert(assertr::not_na, dplyr::everything())
        }
        a$stand_type[[id]] <- stand_type
        a$trees[[id]] <- df[, c("species", "dbh")]

      } else {

        # General checks for seedlings or saplings.
        df <- df |>
          assertr::verify(assertr::has_only_names("species", "n")) |>
          assertr::assert_rows(assertr::num_row_NAs, function(x) x == 0, species, n) |>
          assertr::assert_rows(assertr::col_concat, assertr::is_uniq, species)
        df <- df[, c("species", "n")]


        # Other checks.
        if (data_type == "seedlings") {
          a$seedlings[[id]] <- df |> assertr::assert(assertr::within_bounds(0, 1, allow.na = F), n)
        } else if (data_type == "saplings") {
          a$saplings[[id]] <- df |> assertr::assert(assertr::within_bounds(0, Inf, allow.na = F), n)
        }
      }
    }
  } else if (country == "usa") {
    stop("Calculations for country = 'usa' have not yet been implemented")
  } else if (country == "france") {
    stop("Calculations for country = 'france' have not yet been implemented")
  }


  return(a)
}
