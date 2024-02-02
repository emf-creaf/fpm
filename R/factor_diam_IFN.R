#' Areal factors to convert from IFN plot area to hectare.
#'
#' @description
#' Computation of areal factors for tree plots of variable radius in the
#' Spanish "Inventario Forestal Nacional" (IFN). Optionally, the radius of those plots
#' or the criteria for choosing them is given as output.
#'
#'
#' @param x
#' @param type
#'
#' @details
#' The diameter of tree plots of the Spanish Inventario Forestal Nacional are
#' sampled differently depending on their diameter at breast height (DBH).
#' For a description, call this function with \code{type = "interval"} and
#' \code{x} equal to any value.
#'
#' @return
#' @export
#'
#' @examples
#' factor_diam_IFN(seq(6, 50))
#'
factor_diam_IFN <- function(x, type = "area") {

  stopifnot("Input must be a numeric vector" = (is.vector(x) & is.numeric(x) ))
  type <- match.arg(tolower(type), c("area", "radius", "interval"))


  y <- ifelse(x < 7.5, NA,
              ifelse(x >= 7.5 & x < 12.5, 5,
                     ifelse(x >= 12.5 & x < 22.5, 10,
                            ifelse(x >= 22.5 & x < 42.5, 15, 25))))
  if (type == "area") {
    y <- 10000/(pi*y^2)
  } else {
    y <- data.frame(c("x >= 7.5cm & x < 12.5cm",
                      "x >= 12.5cm & x < 22.5cm",
                      "x >= 22.5cm & x < 42.5cm",
                      "x >= 42.5cm"),
                    paste0(c(5, 10, 15, 25)),
                    10000/(pi*c(5, 10, 15, 25)^2))
    colnames(y) <- c("DBH interval", "Plot radius (m)", "Conversion factor")
  }

  return(y)
}
