#' Remove tree stands
#'
#' @description
#' Remove stands from \code{sf} tree-stand object
#'
#' @param a a \code{sf} object containing a number of POINT geometry types.
#' @param idplot identifiers of POINT elements representing tree stands to remove.
#'
#' @return
#' A \code{sf} object without \code{idplot} stands.
#'
#' @details
#' Simple removal of rows from \code{a}.
#'
#' @examples
#'
#' # First initialize one single stand.
#' a <- start_stand("ID1", 5, 45, "EPSG:4326")
#' a <- set_attributes(a)
#'
#' # Next, we merge other stands.
#' for (i in 2:10) {
#' b <- start_stand(paste0("ID",i), 5, 45, "EPSG:4326")
#' b <- set_attributes(b)
#' a <- merge_stands(a,b)
#' }
#'
#' # Remove two stands.
#' a <- remove_stand(a, c("ID3", "ID8"))
#'
#' @export
#'
remove_stand <- function(a, idplot) {

  i <- match(idplot, a$idplot)
  if (any(is.na(i))) {
    stop(cat(paste0("Could not find ",sum(is.na(i))," plots\n")))
  }

  return(a[-i,])

}
