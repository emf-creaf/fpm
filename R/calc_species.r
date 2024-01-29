#' calc_species
#'
#' @param a \code{list} containing seedlings, saplings and trees information.
#'
#' @description
#' Get species names for seedlings, saplings and trees in a plot \code{list}.
#'
#'
#' @return
#' @export
#'
#' @examples
calc_species <- function(a) {


  if (length(a$trees) > 0) {
    if (a$stand_type == "individual") {
      a$tree_species <- unique(a$trees$species)
    } else if (a$stand_type == "ipm") {
      a$tree_species <- names(a$trees)
    } else {
      stop("Wrong 'stand_type'")
    }
  }
  if (length(a$seedlings) > 0) a$seedling_species <- unique(a$seedlings$species)
  if (length(a$saplings) > 0) a$sapling_species <- unique(a$saplings$species)

  return(a)
}
