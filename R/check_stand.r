#' Check consistency of tree stands
#'
#' @description
#'
#' @param a a \code{sf} object containing a number of POINT geometry types.
#'
#' @return
#' A \code{list} containing the number of sapling and adult tree species presents
#' in the stands.
#'
#' @export
#'
#' @examples
check_stand <- function(a) {

  # Extract species of adult trees present in all plots.
  b <- a %>% pull(trees)
  species_adults <- sapply(1:length(b), function(i) {
    if (is.na(a$stand_type[i])) {
      NA
    } else if (a$stand_type[i] == "individual") {
      unique(b[[i]]$species)
    } else if (a$stand_type[i] == "ipm") {
      colnames(b[[i]])
    }
  })
  species_adults <- unique(unlist(species_adults))

  # Are there NA's in adult tree species? Where?
  adult_sp_NA <- sapply(b, function(x) sum(is.na(x$species)))

  # Are there NA's in adult tree dbh1? Where?
  adult_dbh_NA <- sapply(b, function(x) sum(is.na(x$dbh1)))

  # Same for smaller trees.
  b <- a %>% pull(saplings)
  species_saplings <- unique(unlist(sapply(b, function(x) unique(x$species))))

  sapling_sp_NA <- sapply(b, function(x) sum(is.na(x$species)))

  sapling_N_NA <- sapply(b, function(x) sum(is.na(x$N)))

  return(list(species_adults = species_adults,
              adult_sp_NA = adult_sp_NA,
              adult_dbh_NA = adult_dbh_NA,
              species_saplings = species_saplings,
              sapling_sp_NA = sapling_sp_NA,
              sapling_N_NA = sapling_N_NA))
}
