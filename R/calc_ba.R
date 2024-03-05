#' Calculates basal area per plot and species.
#'
#' @description
#' It calculates the basal area, total and per species, for a plot.
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
calc_ba <- function(a) {


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


  # Calculations.
  ba <- data.frame()
  b <- a$trees[[1]]
  stand_type <- a$stand_type
  cnst <- pi/40000
  if (country == "spain") {
    if (length(b) > 0) {
      if (stand_type == "individual") {
        b$factor_diam <- factor_diam_IFN(b$dbh)
        y <- b |> dplyr::group_by(species) |> dplyr::summarise(ba = sum(factor_diam * dbh^2) * cnst)
        ba <- split(y$ba, y$species)

      } else if (stand_type == "ipm") {
        stopifnot("Inputs 'x' and 'h' are required. Use 'set_parameters' first" = all(!is.null(x) & !is.null(h)))
        ba <- sapply(names(b), function(j) quad(b[[j]]*x[[j]]^2, h[[j]])*cnst, simplify = F)

      } else if (stand_type == "mpm") {
        warning("Stand type 'mpm' not implemented yet")
      } else {
        stop("Wrong 'stand_type'")
      }
    }
  } else if (country == "usa") {
    stop("Calculations for country = 'usa' have not yet been implemented")
  } else if (country == "france") {
    stop("Calculations for country = 'france' have not yet been implemented")
  }


  return(ba)
}
