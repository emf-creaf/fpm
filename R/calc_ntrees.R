#' Calculates number of trees per plot and species.
#'
#' @description
#' It calculates the number of trees, total and per species, for a plot.
#'
#' @param a a \code{sf} object containing a single row.
#'
#' @return
#' @export
#'
#' @details
#' This function is used by \code{update_stands}.
#'
#'
#' @examples
calc_ntrees <- function(a) {


  # Check that input 'a' is an 'sf' object.
  stopifnot("Input 'a' must be an sf object" = inherits(a, "sf"))


  # Check that there is only one row.
  stopifnot("Input 'a' must be a single row" = (nrow(a) == 1))


  # Retrieve parameters. We need the integration variable for the calculations
  # only if any stand is "ipm", but we try to retrieve it nevertheless and check its
  # existence below if stand_type == "ipm".
  p <- a |> get_parameters(c("integvars", "h", "country"))
  country <- p$country
  x <- p$integvars
  h <- p$h


  # If x is not specified, stand_type is individual.
  b <- a$trees[[1]]
  if (a$stand_type == "individual") {
    b$factor_diam <- factor_diam_IFN(b$dbh)
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
