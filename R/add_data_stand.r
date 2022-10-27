#' Title
#'
#' @description
#' Add "trees" or "saplings" data.frames to a single sf object representing a tree stand.
#'
#' @param a
#' @param idplot
#' @param df
#' @param data
#' @param update
#'
#' @return
#'
#' @details
#'
#' @export
#'
#' @examples
#' a <- start_inventory(letters[1:5],runif(5),runif(5),rep("individual",5),runif(5),"EPSG:4326")
#' a <- set_attributes(a,"spain")
#' df <- data.frame(species = c(sample(c("Pnigra","Phalep"),5,replace=T)),dbh1 = 7.5+runif(5)*20, factor_diam1 = sample(c(127.324, 31.83099),5,replace=T))
#' b <- add_stand_data(a, "c", df, "trees")
#'
add_data_stand <- function(a, df, data = "trees") {

  if (nrow(a) > 1) stop("Input 'a' must correspond to a single stand")
  if (!is.data.frame(df)) stop("Input 'df' must be a data.frame")
  if (!any(data == c("trees", "saplings"))) stop("Wrong value in 'data'")

  # Substitute/add data.frame.
  switch(data,
         trees = {a$trees[[1]] <- df},
         saplings = {a$saplings[[1]] <- df}
  )

  return(a)
}
