#' Title
#'
#' @param df
#' @param species
#'
#' @return
#' @export
#'
#' @examples
#'
extract_species <- function(df, species) {

  i <- match(species, colnames(df))
  if (is.na(i)) stop("Species could not be found")

  return(na.omit(df[, c(1,i)]))

}
