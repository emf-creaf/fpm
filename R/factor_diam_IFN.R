#' Areal factors to convert from IFN plot area to hectare.
#'
#' @description
#' Computation of areal factors for tree plots of variable radius in the
#' Spanish "Inventario Forestal Nacional" (IFN). Optionally, the radius of those plots
#' or the criteria for choosing them is given as output.
#'
#' @param x numeric, diameter at breast height in cm.
#'
#' @details
#' The diameter of tree plots of the Spanish Inventario Forestal Nacional are
#' sampled differently depending on their diameter at breast height (DBH).
#' For a description, call this function with \code{type = "interval"} and
#' \code{x} equal to any value.
#'
#' @return
#' A vector with the same length as \code{x} containing the conversion factor to hectare
#' for the Spanish Inventario Forestal Nacional.
#'
#' @export
#'
#' @examples
#' factor_diam_IFN(seq(6, 50))
#'
factor_diam_IFN <- function(x) {

  # Check arguments.
  mf <- match.call()
  m <- match(c("x"), names(mf), 0L)


  # IFN plot radius.
  radi <- c(5, 10, 15, 25)


  # Output.
  if (m == 0) {
    radi <- c(5, 10, 15, 25)
    y <- data.frame(c("x >= 7.5cm & x < 12.5cm",
                      "x >= 12.5cm & x < 22.5cm",
                      "x >= 22.5cm & x < 42.5cm",
                      "x >= 42.5cm"),
                    radi,
                    10000/(pi*radi^2))
    colnames(y) <- c("DBH interval", "Plot radius (m)", "Conversion factor")

  } else {
    stopifnot("Input 'x' must be a numeric vector" = (is.vector(x) & is.numeric(x) ))
    y <- ifelse(x < 7.5, NA,
                ifelse(x >= 7.5 & x < 12.5, 5,
                       ifelse(x >= 12.5 & x < 22.5, 10,
                              ifelse(x >= 22.5 & x < 42.5, 15, 25))))
    y <- 10000/(pi*y^2)
  }


  return(y)
}
