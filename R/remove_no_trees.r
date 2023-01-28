#' Title
#'
#' @param a
#' @param idplot
#'
#' @return
#' @export
#'
#' @examples
remove_no_trees <- function(a, idplot) {

  id <- if (is.null(idplot)) a$idplot else match(idplot, a$idplot)
  if (any(is.na(i))) stop(cat(paste0("Could not find ",sum(is.na(i))," plots\n")))

  j <- NULL
  for (i in id) {
    if (length(a[i, ]$trees[[1]]) == 0) j <- c(j, i)
  }

  return(a[-j,])

}
