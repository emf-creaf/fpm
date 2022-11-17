#' Title
#'
#' @param a
#' @param idplot
#' @param expected_growth
#' @param variance_growth
#' @param min_dbh
#' @param quadrature_type
#'
#' @return
#' @export
#'
#' @examples
ipm_quadrature <- function(a, idplot, expected_growth, variance_growth, survival_prob, min_dbh, quadrature_type = "trapezoidal") {

  id <- match(idplot, a$idplot)
  if (is.na(id)) stop("Could not find 'idplot' in 'a'")
  if (length(id) != 1) stop("Only one 'idplot' can be modified at the time")
  if (a$stand_type[id] != "ipm") stop("'stand_type' must be 'ipm'")

  # Abscissas per species.
  x <- attr(a, "integvars")
  nx <- nrow(x)

  sd_growth <- sqrt(variance_growth)

  # IPM quadrature.
  # if (attr(a, "country") == "spain") {
  #
  #   ng <- length(N)
  #   g.matrix <- matrix(0,ng,ng)
  #   for (i in 1:ng) g.matrix[i,] <- c(rep(0,i-1),dlnorm(y[1:(ng-(i-1))]-min_dbh,meanlog=gr[i],sdlog=sd.va[i]))
  #   Nsu <- N*su
  #   N.new <- sapply(1:ng,function(i) MiscMath::quad_ext_simpson(Nsu*g.matrix[,i],h))
  # }
  # return(N.new)

  # Select plot and species.
  b <- a[id, ]$trees[[1]]
  sp <- colnames(b)

  # Abscissae intervals for species present in the plot.
  h <- unlist(x[2, sp] - x[1, sp])

  for (i in sp) {

    # Expected growth and sd for species 'i'.
    g <- expected_growth[, i]
    s <- sd_growth[, i]

    # Big matrix to store 2D growth term.
    gmat <- matrix(0, nx, nx)
    xx <- x[, i] - min_dbh
    jseq <- 1:nx
    for (j in 1:nx) {
      gmat[j, jseq] <- dlnorm(xx, meanlog = g[j], sdlog = s[j])
      xx <- xx[-length(xx)]
      jseq <- jseq[-1]
    }

    # Former tree distribution times survival.
    Nsu <- b[, i] * survival_prob[, i]

    # Numerical quadrature.
    b[, i] <- sapply(1:nx, function(j) MiscMath::quad_ext_simpson(Nsu*gmat[,j],h[i]))
  }

  # Update trees.
  a[id, ]$trees[[1]] <- b

  return(a)

}
