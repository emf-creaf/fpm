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
build_stand <- function(a, idplot, df,
                        data_type = c("trees", "seedlings", "saplings"),
                        stand_type = c("individual", "ipm"),
                        date = NA,
                        country = c("spain", "usa", "france")) {

  mf <- match.call()
  m <- match(c("a", "idplot", "df", "data_type", "stand_type", "date", "country"), tolower(names(mf)[-1]))
  if (any(is.na(m[1:3]))) stop("Missing 'a' or 'idplot'")


  # Check a.
  stopifnot("Input 'a' must be an sf object" = inherits(a, "sf"))


  # Check 'idplot'. If it does not yet exist it adds a new empty row at the bottom.
  if (length(idplot) != 1) stop("Only one 'idplot' can be modified at the time")
  id <- match(idplot, a$idplot)
  if (is.na(id)) {
    b <- a[1, ]
    b$idplot <- idplot
    b$date <- b$stand_type <- ""
    b$trees <- b$saplings <- b$seedlings <- vector("list", 1)
    a <- rbind(a, b)
    id <- nrow(a)
  }


  # Check data_type.
  data_type <- match.arg(data_type)


  # Check stand_type.
  stand_type <- match.arg(stand_type)


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

      if (data_type == "seedlings") {
        a$seedlings[[id]] <- df |> assertr::assert(assertr::within_bounds(0, 1), n)
      } else if (data_type == "saplings") {
        a$saplings[[id]] <- df |> assertr::assert(assertr::within_bounds(0, Inf), n)
      }
    }
  } else if (country == "usa") {
  } else if (country == "france") {
  }

  return(a)
}
