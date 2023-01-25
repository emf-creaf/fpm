#' Removal of empty POINT
#'
#' @description
#' \code{remove_empty} removes empty POINT rows in a \code{stand} \code{sf} object.
#'
#' @param a a \code{sf} object containing a number of POINT geometry types.
#'
#' @details
#' \code{remove_empty} first uses \code{check_stand} to calculate the number of
#' adult trees, seedlings and saplings, and then removes all rows in the input
#' object with zeros in those three fields. It can be useful in those cases where
#' seed dispersal between tree stands is assumed to be negligible and, thus,
#' an empty stand will always remain empty.
#'
#' @return
#' \code{remove_empty} returns the input \code{sf} object but without rows
#' where the number of adults, seedlings AND saplings are zero.
#'
#' @export
#'
#' @examples
#'
#' # First initialize one single stand.
#' a <- start_stand("ID1", 5, 45, "EPSG:4326")
#' a <- set_attributes(a, country = "spain")
#'
#' # Next, we merge other stands.
#' for (i in 2:10) {
#' b <- start_stand(paste0("ID",i), 5, 45, "EPSG:4326")
#' b <- set_attributes(b, country = "spain")
#' a <- merge_stands(a,b)
#' }
#'
#' # Now we add tree information.
#' for (i in 1:6) {
#' df <- data.frame(species = c(sample(c("Pnigra","Phalep"),5,replace=T)),
#' dbh1 = 7.5+runif(5)*20, factor_diam1 = sample(c(127.324, 31.83099),5,replace=T))
#' a <- build_stand(a, paste0("ID",i), df, "trees", "individual", 1990)
#' }
#'
#'
remove_empty <- function(a) {

  x <- check_stand(a)
  i <- which(x$adults_number == 0 &
               x$seedlings_number == 0 &
               x$saplings_number == 0)
  if(length(i) > 0) a <- a[-i, ]

  return(a)
}
