#' Title
#'
#' @param a
#'
#' @return
#' @export
#'
#' @examples
#' a <- start_inventory(letters[1:5],runif(5),runif(5),rep("individual",5),runif(5),"EPSG:4326")
#' a <- set_attributes(a,"spain")
#' df <- data.frame(species = c(sample(c("Pnigra","Phalep"),5,replace=T)),dbh = 7.5+runif(5)*20, factor_diam = sample(c(127.324, 31.83099),5,replace=T))
#' b <- add_stand_data(a, "c", df, "trees")
#'
add_data_stand <- function(a, idplot, df, data = "trees", update = T) {

  if (length(idplot) > 1) stop("Input 'idplot' must have length = 1")
  i <- match(idplot, a$idplot)
  if (is.na(i)) stop(cat("Stand",idplot,"does not exist"))
  if (!is.data.frame(df)) stop("Input 'df' must be a data.frame")
  if (!any(data == c("trees", "saplings"))) stop("Wrong value in 'data'")

  # Substitute/add data.frame.
  switch(data,
         trees = {a[i, ]$trees[[1]] <- df},
         saplings = {a[i, ]$saplings[[1]] <- df}
  )

  return(a)
}
