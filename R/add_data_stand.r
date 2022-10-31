#' Title
#'
#' @description
#' Add individual "trees" or "saplings" data.frames to a single sf object representing a tree stand.
#'
#' @param a
#' @param data_type
#' @param stand_type
#' @param date
#' @param country
#' @param df
#'
#' @return
#'
#' @details
#'
#' @export
#'
#' @examples
#' a <- start_stand("ID1", 5, 45, "EPSG:4326")
#' a <- set_attributes(a)
#' df <- data.frame(species = c(sample(c("Pnigra","Phalep"),5,replace=T)),
#' dbh1 = 7.5+runif(5)*20, factor_diam1 = sample(c(127.324, 31.83099),5,replace=T))
#' b <- add_data_stand(a, df, "trees", "individual", 1990)
#'
add_data_stand <- function(a, df, data_type, stand_type, date, country = "spain") {

  # Different tests for inputs.
  mf <- match.call()
  m <- match(c("a", "df", "data_type", "stand_type", "date"), tolower(names(mf)[-1]))
  if (any(is.na(m[1:2]))) stop("Missing 'a' or 'df'")
  if (nrow(a) > 1) stop("Input 'a' must correspond to a single stand")
  if (!is.data.frame(df)) stop("Input 'df' must be a data.frame")

  if (!is.na(m[3])) {
    data_type <- match.arg(data_type, choices = c("trees", "saplings"))
  } else {
    stop("Input 'data_type' must be specified")
  }

  if (!is.na(m[4])) {
    stand_type <- match.arg(stand_type, choices = c("individual", "mpm", "ipm"))
    a$stand_type <- stand_type
  }

  if (!is.na(m[5])) a$date <- date

  country <- match.arg(tolower(country), choices = c("spain", "usa", "france"))
  if (country != tolower(attr(a, "country"))) stop("Input 'country' value does not match that of 'a'")

  # if (!any(fpm:::test_country(country),
  #         ifelse(grepl("seedlings", data), fpm:::test_seedlings(data, country),T),
  #         ifelse(grepl("saplings", data), fpm:::test_saplings(data, country),T),
  #         ifelse(data != "trees", fpm:::test_row_species(df),T),
  #         ifelse(data == "trees", fpm:::test_trees_labels(df, country), T),
  #         ifelse(data != "trees", fpm:::test_timepoints_youngs(df, country), T))) stop()




  # Add data.frame to stand.

  if (country == "spain") {
    if (data_type == "trees") {
      a$trees[[1]] <- trees
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

  return(a)
}
