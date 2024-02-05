#' Title
#'
#' @param a
#' @param x
#' @param h
#'
#' @return
#' @export
#'
#' @details
#' This function is used by \code{update_stands}.
#'
#'
#' @examples
calc_ba <- function(a, x = NULL, h = NULL) {


  stopifnot("Input 'a' must be a list" = is.list(a))


  # If x is not specified, stand_type is individual.
  b <- a$trees[[1]]
  if (a$stand_type == "individual") {
    b$factor_diam <- factor_diam_IFN(b$dbh)
    y <- b |> dplyr::group_by(species) |> dplyr::summarise(ba = sum(factor_diam * dbh^2) * (pi/40000))
    ba_species <- setNames(unlist(sapply(1:nrow(y), function(i) y[i, "ba"], simplify = T)), unlist(y[,1]))
  } else if (a$stand_type == "ipm") {

    stopifnot("Inputs 'x' and 'h' are required" = all(!is.null(x) & !is.null(h)))

    ba_species <- sapply(names(b), function(j) quad(b[[j]]*x[[j]]^2, h[[j]])*(pi/40000), simplify = T)
    # a$stats$ba <- sum(unlist(a$stats$ba_species))
  } else if (a$stand_type == "mpm") {
    warning("Stand type 'mpm' not implemented yet")
  } else {
    stop("Wrong 'stand_type'")
  }

  return(data.frame(species = names(ba_species), ba = unname(ba_species)))
}
