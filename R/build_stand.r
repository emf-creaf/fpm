#' Build tree-stand \code{sf} object
#'
#' @description
#' Add or modify most variables of tree-stand \code{sf} object.
#'
#' @param a a \code{sf} object containing a number of POINT geometry types.
#' @param idplot identifier of single POINT, representing a tree stand, to modify.
#' @param df a data frame containing seedling, sapling or tree data.
#' @param data_type string specifying whether the \code{df} data frame corresponds
#' to seedling, sapling or tree data.
#' @param stand_type string specifying which type of tree stand.
#' @param date an optional numeric value setting the sampling date. If missing, the
#' default value will be NA. It can be set at a later time manually.
#' @param country string indicating which country the stand belongs to. It can be
#' \code{country = "spain"}, \code{country = "france"} or \code{country = "usa"}.
#' @param x
#' @param y
#' @param control
#'
#' @return
#' A \code{sf} object with added or modified fields.
#'
#' @details
#' Unlike other functions, \code{build_stand} must be called individually for
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
#' a <- start_stands(c("ID1", "ID2"), c(4, 5), c(45, 45), "EPSG:4326")
#' a <- set_parameters(a, country = "spain")
#'
#' # Next, we merge other stands.
#' for (i in 3:10) {
#'   b <- start_stands(paste0("ID",i), 5, 45, "EPSG:4326")
#'   b <- set_parameters(b, country = "spain")
#'   a <- merge_stands(a,b)
#' }
#'
#' # Now we add tree information.
#' for (i in 1:2) {
#' df <- data.frame(species = c(sample(c("Pnigra","Phalep"),5,replace=T)), dbh = 7.5+runif(5)*20)
#' a <- build_stand(a, paste0("ID",i), df, "trees", "individual", as.Date("1-1-2019", "%d-%m-%Y"))
#' }
#'
build_stand <- function(a = NULL, idplot = NULL, x = NULL, y = NULL, df = NULL,
                        data_type = c("trees", "seedlings", "saplings"),
                        stand_type = c("individual", "ipm"),
                        date = NA,
                        country = c("spain", "usa", "france"),
                        control = list()) {


  # Check idplot.
  stopifnot("Input 'idplot' is missing" = !is.null(idplot))
  stopifnot("Input 'idplot' must have length = 1" = (length(idplot) == 1))


  # 'a' object may have to be created from scratch. If not, check that it is 'sf'.
  if (is.null(a)) {
    a <- start_stands(control)
    a$idplot <- idplot
  } else {
    stopifnot("Input 'a' must be an sf object" = inherits(a, "sf"))
  }


  # If idplot does not yet exist, we add a new empty row at the bottom.
  id <- match(idplot, a$idplot)
  if (is.na(id)) {
    b <- a[1, ]
    b$idplot <- idplot
    b$date <- b$stand_type <- ""
    b$trees <- b$saplings <- b$seedlings <- vector("list", 1)
    a <- rbind(a, b)
    id <- nrow(a)
  }


  # Either coordinates, data or both.
  stopifnot("Either ('x', 'y') or 'df', or both, must be valid" = any(all(!is.null(x), !is.null(y)), !is.null(df)))


  # If coordinates are given, set them.
  if (all(!is.null(x), !is.null(y))) {
    stopifnot("Coordinates must have length = 1 (i.e. it is a single location)" = all(length(x) == 1, length(y) == 1))
    stopifnot("Coordinates must be real numbers" = all(is.numeric(x), is.numeric(y)))
    sf::st_geometry(a) <- sf::st_sfc(
        ifelse(a$idplot == idplot, sf::st_sfc(sf::st_point(c(x, y))), a$geometry),
        crs = sf::st_crs(a$geometry))
  }


  # Check data_type.
  data_type <- match.arg(data_type)


  # Check stand_type.
  stand_type <- match.arg(stand_type)


  # Valid df.
  if (exists('df', mode = "data.frame")) {

    # Check stand_type depending on the data_type value.
    if (data_type == "trees") {
      stopifnot("'stand_type' value must be 'individual' or 'ipm' if 'data_type' is 'trees'" = any(stand_type == c("individual", "ipm")))
    }

    # Check 'df' for data.frame or list.
    if (stand_type == "individual") {
      stopifnot("Input 'df' must be a data.frame when 'stand_type' = 'individual'" = inherits(df, "data.frame"))
    } else if (stand_type == "ipm") {
      stopifnot("Input 'df' must be a list when 'stand_type' = 'ipm'" = inherits(df, "list"))
    }
  }


  # Check date.
  stopifnot("Input 'date' must be an object of class 'Date'" = inherits(date, "Date"))
  a$date[[id]] <- date


  # Check country.
  country <- match.arg(country)
  stopifnot("Input 'country' must match the 'country' attribute of 'a'" = attr(a, "country") == country)


  # Checks that carried out below:
  # - There are no NA values, neither in trees nor in seedlings/saplings.
  # - Tree dbh is always > 0.
  # - Number of seedlings/saplings is always > 0.
  # - Seedlings/saplings are not duplicated.
  if (!is.null(df)) {
    if (country == "spain") {
      if (data_type == "trees") {
        a$stand_type[[id]] <- stand_type
        if (stand_type == "individual") {
          df <- df |>
            assertr::verify(assertr::has_only_names("species", "dbh")) |>
            assertr::assert_rows(assertr::num_row_NAs, function(x) x == 0, dbh) |>
            assertr::verify(dbh > 0)
        } else {
          df <- df |> assertr::assert(is_larger(0), dplyr::everything()) |>
            assertr::assert(assertr::not_na, dplyr::everything())
        }
        a$trees[[id]] <- df[, c("species", "dbh")]

      } else {
        df <- df |>
          assertr::verify(assertr::has_only_names("species", "n")) |>
          assertr::assert_rows(assertr::num_row_NAs, function(x) x == 0, species, n) |>
          assertr::assert_rows(assertr::col_concat, assertr::is_uniq, species)
        df <- df[, c("species", "n")]

        f <- function() stop("Error!")
        if (data_type == "seedlings") {
          a$seedlings[[id]] <- df |> assertr::assert(assertr::within_bounds(0, 1), n, error_fun = f)
        } else if (data_type == "saplings") {
          a$saplings[[id]] <- df |> assertr::assert(assertr::within_bounds(0, Inf), n, error_fun = f)
        }
      }
    } else if (country == "usa") {
    } else if (country == "france") {
    }
  }

  return(a)
}
