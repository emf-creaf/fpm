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
#' labeled "species", "dbh1" and "factor_diam1". When it is saplings that want
#' to add to \code{a}, that \code{data.frame} must have two columns, labeled
#' "species" and "N".
#'
#' @export
#'
#' @examples
#'
#' # First initialize one single stand.
#' a <- start_stand("ID1", 5, 45, "EPSG:4326")
#' a <- set_attributes(a)
#'
#' # Next, we merge other stands.
#' for (i in 2:10) {
#' b <- start_stand(paste0("ID",i), 5, 45, "EPSG:4326")
#' b <- set_attributes(b)
#' a <- merge_stands(a,b)
#' }
#'
#' # Now we add tree information.
#' for (i in 1:10) {
#' df <- data.frame(species = c(sample(c("Pnigra","Phalep"),5,replace=T)),
#' dbh1 = 7.5+runif(5)*20, factor_diam1 = sample(c(127.324, 31.83099),5,replace=T))
#' a <- build_stand(a, paste0("ID",i), df, "trees", "individual", 1990)
#' }
#'
build_stand <- function(a, idplot, df,
                        data_type = c("trees", "seedlings", "saplings"),
                        stand_type = c("individual", "mpm", "ipm"),
                        date = NA,
                        country = c("spain", "usa", "france")) {

  mf <- match.call()
  m <- match(c("a", "idplot", "df", "data_type", "stand_type", "date", "country"), tolower(names(mf)[-1]))

  # Does 'idplot' exist?
  if (any(is.na(m[1:2]))) stop("Missing 'a' or 'idplot'")
  id <- match(idplot, a$idplot)
  if (is.na(id)) stop("Could not find 'idplot' in 'a'")
  if (length(id) != 1) stop("Only one 'idplot' can be modified at the time")
  if (!is.data.frame(df)) stop("Input 'df' must be a data.frame")

  # Check choices of arguments.
  data_type <- match.arg(data_type)
  a$stand_type[[id]] <- match.arg(stand_type)
  a$date <- date
  country <- match.arg(country)
  if (attr(a, "country") != country) stop("Attribute 'country' does not match")

  # Update seedlings, saplings or trees.
  if (country == "spain") {
    if (data_type == "trees") {
      a$trees[[id]] <- df
    } else {
      if (any(duplicated(df))) stop("There are duplicated rows in data.frame 'df'")
      if (data_type == "seedlings") {
        a$seedlings[[id]] <- df
      } else if (data_type == "saplings") {
        a$saplings[[id]] <- df
      }
    }
  }

  return(a)
}
