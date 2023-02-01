#' Title
#'
#' @param obj
#' @param newdata
#'
#' @return
#' @export
#'
#' @examples
saplings_model <- function(obj, newdata) {
  return(predict(obj, newdata = newdata, type = "response"))
}
