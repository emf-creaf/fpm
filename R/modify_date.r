#' Title
#'
#' @param a
#' @param idplot
#' @param date
#'
#' @return
#' @export
#'
#' @examples
modify_date <- function(a, idplot, date) {

  id <- match(idplot, a$idplot)
  if (any(is.na(id))) stop("Some plots cannot be found in 'a'")
  if (is.null(date)) stop("Missing date")
  ldate <- length(date)
  if (ldate == 1) {
    date <- rep(date, length(id))
  } else {
    if (ldate != length(id)) stop("Length of 'idplot' and 'date' do not match")
  }

  # Insert date.
  a$date[id] <- date

  return(a)
}
