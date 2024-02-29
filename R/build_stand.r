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
#' trees (that is, when \code{data_type = "trees"}) must have three columns,
#' labeled "species", "dbh" and "factor_diam". When it is saplings that we want
#' to add to \code{a}, its format must be long, i.e. the input \code{data.frame} must have two columns, labeled
#' "species" and "N".
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
#' # Next, we add two stands.
#' df <- data.frame(species = c('Pinus halepensis', 'Quercus ilex'), dbh = c(8.6, 12.7))
#' a <- build_stand(a, "id1", data = list(df = df))
#'
#' for (i in 3:10) {
#'   b <- start_stands(paste0("ID",i), 5, 45, "EPSG:4326")
#'   b <- set_parameters(b, list(country = "spain")
#'   a <- merge_stands(a,b)
#' }
#'
#' # Now we add tree information.
#' for (i in 1:2) {
#' df <- data.frame(species = c(sample(c("Pnigra","Phalep"),5,replace=T)), dbh = 7.5+runif(5)*20)
#' a <- build_stand(a, paste0("ID",i), df, "trees", "individual", as.Date("1-1-2019", "%d-%m-%Y"))
#' }
#'
build_stand <- function(a, idplot = NULL, data = list(), verbose = T) {

  # Check that input 'a' is an 'sf' object.
  stopifnot("Input 'a' must be an sf object" = inherits(a, "sf"))


  # Check idplot. It must be provided.
  stopifnot("Input 'idplot' is missing" = !is.null(idplot))
  stopifnot("Input 'idplot' must have length = 1" = (length(idplot) == 1))


  # Check idplot. If NULL, return with a warning.
  stopifnot("Input 'data' must be a list" = inherits(data, "list"))
  if (length(data) == 0) {
    warning("Empty 'data' input list. Returning 'a'")
    return(a)
  }


  # Parameters. For now, only country = spain" is implemented.
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
  }


  # Either coordinates, data or both.
  stopifnot("Either ('x', 'y') or 'df', or both, must be valid" = any(all(!is.null(x), !is.null(y)), !is.null(df)))


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
        if (stand_type == "individual") {
          stopifnot("Input 'df' must be a data.frame when 'stand_type' = 'individual'" = inherits(df, "data.frame"))
        } else if (stand_type == "ipm") {
          stopifnot("Input 'df' must be a list when 'stand_type' = 'ipm'" = inherits(df, "list"))
        }
      }

  # Checks that are carried out below:
  # - There are no NA values, neither in trees nor in seedlings/saplings.
  # - Tree dbh is always > 0.
  # - Number of seedlings/saplings is always > 0.
  # - Seedlings/saplings are not duplicated.

      if (data_type == "trees") {
        if (stand_type == "individual") {
          df <- df |>
            assertr::verify(assertr::has_only_names("species", "dbh")) |>
            assertr::assert_rows(assertr::num_row_NAs, function(x) x == 0, dbh) |>
            assertr::verify(dbh > min_dbh)
        } else {
          df <- df |> assertr::assert(is_larger(min_dbh), dplyr::everything()) |>
            assertr::assert(assertr::not_na, dplyr::everything())
        }
        a$stand_type[[id]] <- stand_type
        a$trees[[id]] <- df[, c("species", "dbh")]

      } else if (data_type == "individual") {
        df <- df |>
          assertr::verify(assertr::has_only_names("species", "n")) |>
          assertr::assert_rows(assertr::num_row_NAs, function(x) x == 0, species, n) |>
          assertr::assert_rows(assertr::col_concat, assertr::is_uniq, species)
        df <- df[, c("species", "n")]

      }

      ff <- function() stop("Error!")
      if (data_type == "seedlings") {
        a$seedlings[[id]] <- df |> assertr::assert(assertr::within_bounds(0, 1, allow.na = F), n, error_fun = ff)
      } else if (data_type == "saplings") {
        a$saplings[[id]] <- df |> assertr::assert(assertr::within_bounds(0, Inf, allow.na = F), n, error_fun = ff)
      }
    }
  }


  return(a)
}
