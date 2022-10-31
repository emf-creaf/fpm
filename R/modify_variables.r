#' Title
#'
#' @param a
#' @param idplot
#'
#' @return
#' @export
#'
#' @examples
#' a <- start_stand("ID1", 5, 45, "EPSG:4326")
#' a <- set_attributes(a)
#' df <- data.frame(species = c(sample(c("Pnigra","Phalep"),5,replace=T)),
#' dbh1 = 7.5+runif(5)*20, factor_diam1 = sample(c(127.324, 31.83099),5,replace=T))
#' b <- add_data_stand(a, df, "trees", "individual", 1990)
#' b <- modify_variables(b, "ID1", data_type = "saplings")
#'
modify_variables <- function(a, idplot, df, data_type, stand_type, date) {

  mf <- match.call()
  m <- match(c("a", "idplot", "df", "data_type", "stand_type", "date"), tolower(names(mf)[-1]))

  if (any(is.na(m[1:2]))) stop("Missing 'a' or 'idplot'")
  i <- match(idplot, a$idplot)
  if (is.na(i)) stop("Could not find 'idplot' in 'a'")

  if (!is.na(m[4])) a[i, ]$data_type <- match.arg(data_type, choices = c("trees", "saplings"))
  if (!is.na(m[5])) a[i, ]$stand_type <- match.arg(stand_type, choices = c("individual", "mpm", "ipm"))
  if (!is.na(m[6])) a[i, ]$date <- date

  if (!is.na(m[3])) {
    if (!is.data.frame(df)) stop("Input 'df' must be a data.frame")
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
  }

  return(a)

}
