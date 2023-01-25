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
    } else {
      if (a$stand_type[i] == "individual") {
        unique(b[[i]]$species)
      } else if (a$stand_type[i] == "ipm") {
        colnames(b[[i]])
      }
    }
  })

  species_adults <- unique(na.omit(unlist(species_adults)))

  # How many species per plot?
  adults_species_number <- sapply(b, function(x) sum(!is.na(unique(x$species))))

  # How many NA's per plot in species name?
  adults_species_NA <- sapply(b, function(x) sum(is.na(x$species)))

  # How many different non-NA adult trees per plot?
  adults_number <- sapply(b, function(x) sum(!is.na((x$dbh1))))

  # How many NA's per plot in dbh?
  adults_dbh_NA <- sapply(b, function(x) sum(is.na(x$dbh1)))

  # Same for seedlings and saplings.
  seedlings <- a %>% pull(seedlings) %>% stat_minor_trees()
  saplings <- a %>% pull(saplings) %>% stat_minor_trees()

  return(list(species_adults = species_adults,
              adults_species_number = adults_species_number,
              adults_species_NA = adults_species_NA,
              adults_number = adults_number,
              adults_dbh_NA = adults_dbh_NA,

              species_seedlings = seedlings$species,
              seedlings_species_number = seedlings$species_number,
              seedlings_species_NA = seedlings$species_NA,
              seedlings_number = seedlings$number,
              seedlings_number_NA = seedlings$number_NA,

              species_saplings = saplings$species,
              saplings_species_number = saplings$species_number,
              saplings_species_NA = saplings$species_NA,
              saplings_number = saplings$number,
              saplings_number_NA = saplings$number_NA))
}
