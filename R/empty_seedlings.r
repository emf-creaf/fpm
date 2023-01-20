#' Set seedlings to an empty \code{data.frame}
#'
#' @param a
#' @param idplot
#'
#' @return
#' @export
#'
#' @examples
empty_seedlings <- function(a, idplot) {

  mf <- match.call()
  m <- match(c("a", "idplot"), tolower(names(mf)[-1]))
  if (any(is.na(m))) stop("Missing input")

  i <- match(idplot, a$idplot)
  if (any(is.na(i))) stop(cat(paste0("Could not find ",sum(is.na(i))," plots\n")))

  a$saplings[[i]] <- data.frame()


  return(a)
}
