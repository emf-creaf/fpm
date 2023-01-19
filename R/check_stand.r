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

  # How many species per plot?
  adult_species_number <- sapply(b, function(x) sum(!is.na(unique(x$species))))

  # How many NA's per plot in species name?
  adult_species_NA <- sapply(b, function(x) sum(is.na(x$species)))

  # How many different non-NA adult trees per plot?
  adult_tree_number <- sapply(b, function(x) sum(!is.na((x$dbh1))))

  # How many NA's per plot in dbh?
  adult_dbh_NA <- sapply(b, function(x) sum(is.na(x$dbh1)))

  # Same for smaller trees.
  b <- a %>% pull(saplings)
  species_saplings <- unique(unlist(sapply(b, function(x) unique(x$species))))
  sapling_species_number <- sapply(b, function(x) sum(!is.na(unique(x$species))))
  sapling_species_NA <- sapply(b, function(x) sum(is.na(x$species)))
  sapling_number <- sapply(b, function(x) sum(x$N, na.rm=T))
  sapling_number_NA <- sapply(b, function(x) sum(is.na(x$N)))

  return(list(species_adults = species_adults,
              adult_species_number = adult_species_number,
              adult_species_NA = adult_species_NA,
              adult_tree_number = adult_tree_number,
              adult_dbh_NA = adult_dbh_NA,
              species_saplings = species_saplings,
              sapling_species_number = sapling_species_number,
              sapling_species_NA = sapling_species_NA,
              sapling_number = sapling_number,
              sapling_number_NA = sapling_number_NA))
}
