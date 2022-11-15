#' Title
#'
#' @param a
#' @param idplot
#' @param quadrature_type
#'
#' @return
#' @export
#'
#' @examples
ipm_quadrature <- function(a, idplot, gr, va, quadrature_type = "trapezoidal") {



  sd.va <- sqrt(va)

  # IPM quadrature.
  if (attr(a, "country") == "spain") {

    ng <- length(N)
    g.matrix <- matrix(0,ng,ng)
    for (i in 1:ng) g.matrix[i,] <- c(rep(0,i-1),dlnorm(y[1:(ng-(i-1))]-min_dbh,meanlog=gr[i],sdlog=sd.va[i]))
    Nsu <- N*su
    N.new <- sapply(1:ng,function(i) MiscMath::quad_ext_simpson(Nsu*g.matrix[,i],h))
  }
  return(N.new)
}
