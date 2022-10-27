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

modify_stand <- function(a, idplot, df, data = "trees") {

  i <- match(b$idplot, a$idplot)
  if (is.na(i)) stop("Could not find 'idplot'in a")

  # Modify.
  switch(data,
         trees = {a[idplot, ]$trees[[1]] <- df},
         saplings = {a[idplot, ]$saplings[[1]] <- df}
  )

  return(a)

}
