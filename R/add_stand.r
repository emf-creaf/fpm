#' Add new tree-stand to tree-stand \code{sf} object
#'
#' @description
#' Create and add or modify most variables of tree-stand \code{sf} object.
#'
#' @param a a \code{sf} object containing a number of POINT geometry types.
#' @param idplot identifier of single POINT, representing a tree stand, to modify.
#' @param df a data frame containing seedling, sapling or tree data.
#' @param data_type string specifying whether the \code{df} data frame corresponds
#' to seedling, sapling or tree data.
#' @param stand_type string specifying which type of tree stand, "individual" or "ipm".
#' @param date an optional numeric value setting the sampling date. If missing, the
#' default value will be NA. It can be set at a later time manually.
#'
#' @return
#' A \code{sf} object with added or modified fields.
#'
#' @details
#' Unlike, \code{add_stand} does not check for the \country{parameter}. It assumes that
#' it is correct. Moreover, if the stand "idplot already exists
#'
#' @export
#'
#' @examples
#'
#' # First initialize stands.
#' a <- start_stands(c("ID1", "ID2"), c(4, 5), c(45, 45), "EPSG:4326")
#' a <- set_parameters(a, country = "spain")
#'
#' # Next, we add other stands.
#' for (i in 3:10) {
#'   b <- start_stands(paste0("ID",i), 5, 45, "EPSG:4326")
#'   b <- set_parameters(b, country = "spain")
#'   a <- add_stands(a)
#' }
#'
#' # Now we add tree information.
#' for (i in 1:2) {
#' df <- data.frame(species = c(sample(c("Pnigra","Phalep"),5,replace=T)), dbh = 7.5+runif(5)*20)
#' a <- build_stand(a, paste0("ID",i), df, "trees", "individual", as.Date("1-1-2019", "%d-%m-%Y"))
#' }
#'
add_stand <- function(a, idplot, df,
                        data_type = c("trees", "seedlings", "saplings"),
                        stand_type = c("individual", "ipm"),
                        date = NA) {

  mf <- match.call()
  m <- match(c("a", "idplot", "df", "data_type", "stand_type", "date"), tolower(names(mf)[-1]))
  if (any(is.na(m[1:3]))) stop("Missing 'a' or 'idplot'")

  # Input must be a "sf" object.
  stopifnot("Input 'a' must be an sf object" = inherits(a, "sf"))


  # Check 'idplot'. It must not exist in 'a' and cannot be a vector.
  if (length(idplot) != 1) stop("Only one 'idplot' can be added at the time")
  id <- match(idplot, a$idplot)
  stopifnot("There is already a plot in 'a' with that identifier" = is.na(id))


  # Check data_type and date.
  data_type <- match.arg(data_type)
  stopifnot("Input 'date' must be an object of class 'Date'" = inherits(date, "Date"))


  # Check stand_type.
  stand_type <- match.arg(stand_type)
  if (stand_type == "ipm") stopifnot("Input 'df' must be a 'list'" = is.list(df))


  # Check data_type.
  a$stand_type[[id]] <- match.arg(stand_type)


  # If everything ok, add date.
  a$date <- date


  # Checks that carried out below:
  # - There are no NA values, neither in trees nor in seedlings/saplings.
  # - Tree dbh is always > 0.
  # - Number of seedlings/saplings is always > 0.
  # - Seedlings/saplings are not duplicated.

  if (country == "spain") {
    if (data_type == "trees") {
      if (stand_type == "individual") {
        stopifnot("Input 'df' must be a data.frame" = is.data.frame(df))
        df <- df |>
          assertr::verify(assertr::has_only_names("dbh", "species")) |>
          assertr::assert_rows(assertr::num_row_NAs, function(x) x == 0, dbh) |>
          assertr::verify(dbh > 0)

      } else if (stand_type == "ipm") {
        df <- df |> assertr::assert(is_larger(0), dplyr::everything()) |>
          assertr::assert(assertr::not_na, dplyr::everything())
      }

      a$trees[[id]] <- df

    } else {
      stopifnot("Input 'df' must be a data.frame" = is.data.frame(df))

      if (data_type == "seedlings") {

        a$seedlings[[id]] <- df |>
          assertr::verify(assertr::has_only_names("species", "n")) |>
          assertr::assert_rows(assertr::num_row_NAs, function(x) x == 0, species, n) |>
          assertr::assert_rows(assertr::col_concat, assertr::is_uniq, species) |>
          assertr::assert(assertr::within_bounds(0, 3), n)

      } else if (data_type == "saplings") {

        a$saplings[[id]] <- df |>
          assertr::verify(assertr::has_only_names("species", "n")) |>
          assertr::assert_rows(assertr::num_row_NAs, function(x) x == 0, species, n) |>
          assertr::assert_rows(assertr::col_concat, assertr::is_uniq, species) |>
          assertr::assert(assertr::within_bounds(0, Inf), n)

      }
    }
  } else if (country == "usa") {
  } else if (country == "france") {
  }

  return(a)
}
