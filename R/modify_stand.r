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

modify_stand <- function(a, idplot, df, data_type, stand_type, date) {

    mf <- match.call()
    m <- match(c("a", "idplot", "df", "data_type", "stand_type", "date"), tolower(names(mf)))
    if (any(is.na(m[1:2]))) stop("Missing 'a' or 'idplot'")
    i <- match(idplot, a$idplot)
    if (is.na(i)) stop("Could not find 'idplot' in 'a'")

    if (!is.na(df))

    if (!is.na(m[3])) a[i, ]$data_type <- data_type
    if (!is.na(m[4])) a[i, ]$stand_type <- stand_type
    if (!is.na(m[5])) a[i, ]$date <- date


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
