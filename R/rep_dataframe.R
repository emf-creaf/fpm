#' Repeat rows in a data.frame
#'
#' @description
#' It repeats a 1-row data.frame a number of times.
#'
#' @param df a 1-row \code{data.frame} to be repeated row-wise.
#' @param times an integer single number specifying the number of times to repeat \code{df}.
#'
#' @details
#' Little function seen in https://stackoverflow.com/questions/11121385/repeat-rows-of-a-data-frame.
#'
#' @return
#' A \code{data.frame} with \code{times} rows that repeat the values in \code{df}.
#'
#' @export
#'
#' @examples
#' df <- data.frame(x = 4, y = 6)
#' newdf <- rep_dataframe(df, 4)
#'
rep_dataframe <- function(df, times) {

  stopifnot("Input 'df' must be a data.frame" = is.data.frame(df))
  stopifnot("Input data.frame 'df' must have one row only" = nrow(df) == 1)
  stopifnot("Input 'times' must be a positive natural number" = (round(times) == times) & (times > 0) & (length(times) == 1))
  return(as.data.frame(lapply(df, rep, times)))

}
