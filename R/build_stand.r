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
#' @param date date that corresponds to the
#' @param country string indicating which country the stand belongs to. It can be
#' \code{country = "spain"}, \code{country = "france"} or \code{country = "usa"}.
#'
#' @return
#' A \code{sf} object with added or modified fields.
#'
#' @export
#'
#' @examples
#' a <- start_stand("ID1", 5, 45, "EPSG:4326")
#' a <- set_attributes(a)
#' df <- data.frame(species = c(sample(c("Pnigra","Phalep"),5,replace=T)),
#' dbh1 = 7.5+runif(5)*20, factor_diam1 = sample(c(127.324, 31.83099),5,replace=T))
#' a <- build_stand(a, "ID1", df, "trees", "individual", 1990)
#'
build_stand <- function(a, idplot, df, data_type, stand_type, date, country = "spain") {

  mf <- match.call()
  m <- match(c("a", "idplot", "df", "data_type", "stand_type", "date", "country"), tolower(names(mf)[-1]))

  # Does 'idplot' exist?
  if (any(is.na(m[1:2]))) stop("Missing 'a' or 'idplot'")
  i <- match(idplot, a$idplot)
  if (is.na(i)) stop("Could not find 'idplot' in 'a'")
  if (length(i) != 1) stop("Only one 'idplot' can be modified at the time")

  # Check choices of arguments.
  if (!is.na(m[4])) a$data_type <- match.arg(data_type, choices = c("trees", "saplings"))
  if (!is.na(m[5])) a$stand_type <- match.arg(stand_type, choices = c("individual", "mpm", "ipm"))
  country <- match.arg(tolower(country), choices = c("spain", "usa", "france"))
  if (country != tolower(attr(a, "country"))) stop("Input 'country' value does not match that of 'a'")

  # If 'date' exists.
  if (!is.na(m[6])) a$date <- date

  # If 'df' exists, seedlings, saplings or trees will change.
  if (!is.na(m[3])) {
    if (!is.data.frame(df)) stop("Input 'df' must be a data.frame")
    if (country == "spain") {
      if (data_type == "trees") {
        a$trees[[1]] <- df
      } else {
        if (any(grepl(data_type, paste0("seedlings", num_seedlings())))) {
          i <- grep(data_type, paste0("seedlings", num_seedlings()))
          a$seedlings[[i]] <- df
        } else if (any(grepl(data_type, paste0("saplings", num_saplings())))) {
          i <- grep(data_type, paste0("saplings", num_saplings()))
          a$saplings[[i]] <- df
        }
      }
    }
  }

  return(a)
}
