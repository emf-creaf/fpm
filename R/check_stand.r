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

  # First extract species of adult trees present in all plots.
  b <- a %>% pull(trees)
  adultsp <- sapply(1:length(b), function(i) {
    if (is.na(a$stand_type[i])) {
      NA
    } else if (a$stand_type[i] == "individual") {
      unique(b[[i]]$species)
    } else if (a$stand_type[i] == "ipm") {
      colnames(b[[i]])
    }
  })
  adultsp <- unique(unlist(adultsp))

  # adultsp <- unique(unlist(sapply(b, function(x) switch(a$stand_type,
  #                                                            individual = unique(x$species),
  #                                                            ipm = colnames(x))
  # )))

  # Same for smaller trees.
  b <- a %>% pull(saplings)
  saplingsp <- unique(unlist(sapply(b, function(x) unique(x$species))))

  return(list(species.adults = adultsp,
              species.saplingsp = saplingsp))
}
