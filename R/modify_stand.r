#' Title
#'
#' @param a
#' @param idplot
#' @param b
#'
#' @return
#' @export
#'
#' @examples
#'
#'
# modify_stand <- function(a, b) {
#
#   i <- match(b$idplot, a$idplot)
#   if (is.na(i)) stop("Could not find 'idplot' of b in a")
#
#   # Eliminate and merge.
#   a <- a[-i, ]
#   a <- fpm::merge_stands(a, b)
#
#   return(a)
#
# }

modify_stand <- function(a, idplot, df, data = "trees", date = NULL) {

  if (length(idplot) > 1) stop("Only one plot at the time")
  if (is.na(match(idplot, a$idplot))) stop("Could not find 'idplot'in a")




  # Modify.
  if (!is.null(date)) a[idplot, ]$date = date

  switch(data,
         trees = {a[idplot, ]$trees[[1]] <- df},
         saplings = {a[idplot, ]$saplings[[1]] <- df}
  )

  return(a)

}
