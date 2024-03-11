# Functions to check lower or upper bounds in "assert" call.
is_larger <- function(bound, include.bound = F) {
  the_call <- deparse(sys.call())
  fun <- function(x) {
    if (is.null(x)) stop("bounds must be checked on non-null element")
    if (!is.numeric(x)) stop("bounds must only be checked on numerics")
    operator <- if (!include.bound) `>` else `>=`
    return(operator(x, bound) & !is.na(x))
  }

  attr(fun, "assertr_vectorized") <- TRUE
  attr(fun, "call") <- the_call
  return(fun)
}

is_smaller <- function(bound, include.bound = F) {
  the_call <- deparse(sys.call())
  fun <- function(x) {
    if (is.null(x)) stop("bounds must be checked on non-null element")
    if (!is.numeric(x)) stop("bounds must only be checked on numerics")
    operator <- if (!include.bound) `<` else `<=`
    return(operator(x, bound) & !is.na(x))
  }

  attr(fun, "assertr_vectorized") <- TRUE
  attr(fun, "call") <- the_call
  return(fun)
}




quad <- function(y, h, type = 1) {

  if (!is.vector(y) & !is.matrix(y)) stop("y must be a vector or a matrix")
  if (!any(type %in% c(1, 2))) stop("Input 'type' must be equal to 1 or 2")

  ny <- ifelse(is.vector(y),length(y),nrow(y))

  if (type == 1) {
    if (ny < 6) stop("y must have length (if it is a vector) or number of rows (if it is a matrix) equal to or larger than 6")
    if (is.vector(y)) {
      q <- sum(y) - 5/8*(y[1]+y[ny]) + 1/6*(y[2]+y[ny-1]) - 1/24*(y[3]+y[ny-2])
    } else {
      q <- colSums(y) - 5/8*(y[1,]+y[ny,]) + 1/6*(y[2,]+y[ny-1,]) - 1/24*(y[3,]+y[ny-2,])
    }
  } else {
    if (ny < 8) stop("y must have length (if it is a vector) or number of rows (if it is a matrix) equal to or larger than 8")
    if (is.vector(y)) {
      q <- sum(y) - 31/48*(y[1]+y[ny]) + 11/48*(y[2]+y[ny-1]) - 5/48*(y[3]+y[ny-2]) + 1/48*(y[4]+y[ny-3])
    } else {
      q <- colSums(y) - 31/48*(y[1,]+y[ny,]) + 11/48*(y[2,]+y[ny-1,]) - 5/48*(y[3,]+y[ny-2,]) + 1/48*(y[4,]+y[ny-3,])
    }
  }

  return(h*q)

}



# Little function seen in https://stackoverflow.com/questions/11121385/repeat-rows-of-a-data-frame
# and needed below.
rep_dataframe <- function(x, times) as.data.frame(lapply(x, rep, times))

