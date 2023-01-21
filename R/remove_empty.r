#' Title
#'
#' @param a
#'
#' @return
#' @export
#'
#' @examples
remove_empty <- function(a) {

  x <- check_stand(a)
  idplot <- which(x$adult_number == 0 &
                    x$seedlings_number == 0 &
                    x$saplings_number == 0)
  if(length(idplot) > 0) a <- a[-idplot, ]
  return(a)
}
