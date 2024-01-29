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
calc_ntrees <- function(a, x = NULL, h = NULL) {


  stopifnot("Input 'a' must be a list" = is.list(a))


  # If x is not specified, stand_type is individual.
  b <- a$trees[[1]]
  if (a$stand_type == "individual") {
    y <- b |> dplyr::group_by(species) |> dplyr::summarise(nt = sum(factor_diam))
    ntrees_species <- setNames(unlist(sapply(1:nrow(y), function(i) y[i, "nt"])), unlist(y[,1]))
  } else if (a$stand_type == "ipm") {
    ntrees_species <- sapply(names(b), function(j) quad(b[[j]], h[[j]]), simplify = T)
  } else if (a$stand_type == "mpm") {
    warning("Stand type 'mpm' not implemented yet")
  } else {
    stop("Wrong 'stand_type'")
  }


  return(data.frame(species = names(ntrees_species), ntrees = unname(ntrees_species)))
}
