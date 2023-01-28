#' Title
#'
#' @param a
#' @param idplot
#'
#' @return
#' @export
#'
#' @examples
remove_no_saplings <- function(a, idplot = NULL) {

  id <- if (is.null(idplot)) 1:length(a$idplot) else match(idplot, a$idplot)
  if (any(is.na(i))) stop(cat(paste0("Could not find ",sum(is.na(i))," plots\n")))

  j <- NULL
  for (i in id) {
    if (length(a$saplings[[i]]) == 0) j <- c(j, i)
  }

  if (!is.null(j)) a <- a[-j, ]
  return(a[-j,])

}
