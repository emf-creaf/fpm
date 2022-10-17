#' Title
#'
#' @param a
#'
#' @return
#' @export
#'
#' @examples
#' a <- start_inventory(letters[1:5],runif(5), runif(5),"EPSG:4326","Spain")
#' a <- set_type(a)
#' df <- data.frame(species = c(sample(c("Pnigra","Phalep"),5,replace=T)),dbh = 7.5+runif(5)*20, factor_diam = sample(c(127.324, 31.83099),5,replace=T))
#' b <- add_stand_data(a,"c",df,"Spain","trees")
#'
#'
add_stand_data <- function(a, idplot, df, country = "Spain", data = "trees", update = T) {


  if (length(idplot) > 1) stop("Input 'idplot' must have length = 1")
  i <- match(idplot, a$idplot)
  if (is.na(i)) stop(cat("Stand",idplot,"does not exist"))
  if (!is.data.frame(df)) stop("Input 'df' must be a data.frame")
  if (!any(country %in% c("Spain", "USA", "France"))) stop("Wrong 'country' value")

  if (!any(data == c("trees", "saplings"))) stop("Wrong value in 'type'")

  # Check country.
  if (!(country %in% class(a))) stop("Wrong 'country' value")

  # Check that abscissas match when datasets are not individual trees.
  if (attr(a, "type") == "matrix") {
  }
  if (attr(a, "type") == "continuous") {
    if (data == "trees") {
      x <- attr(a, "abscissas")
    }
  }

  # TODO: Filter with dplyr/tidyr and check that length is the same as x for all species.!!!!!!!

  # Substitute/add data.frame.
  switch(data,
         trees = {a[i, ]$trees[[1]] <- df},
         saplings = {a[i, ]$saplings[[1]] <- df}
  )

  return(a)
}
