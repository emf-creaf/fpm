#' Title
#'
#' @param a
#' @param idplot
#' @param reg
#' @param lambda
#' @param scaled_ha
#'
#' @return
#' @export
#'
#' @examples
ipm_ingrowth <- function(a, idplot, reg, lambda, scaled_ha = T) {

  # Check idplot.
  id <- match(idplot, a$idplot)
  if (is.na(id)) stop("Could not find 'idplot' in 'a'")
  if (length(id) != 1) stop("Only one 'idplot' can be modified at the time")
  if (a$stand_type[id] != "ipm") stop("'stand_type' must be 'ipm'")

  b <-


  q[,j] <- MiscStat::dtrexp(stands[[iTime]]$dbh[,j],lambda.ingrowth[j],min=min_DBH)*
    # ipm_ingrowth(reg.zero.one[[j]],reg.ingrowth[[j]],cbind(dat,sapl=sub.pm[i,j]))
    ipm_ingrowth(reg.ingrowth[[j]],cbind(dat,sapl=sub.pm[i,j]))


}
