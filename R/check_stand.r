#' Check consistency of tree stands
#'
#' @description
#'
#' @param a a \code{sf} object containing a number of POINT geometry types.
#'
#' @return
#' @export
#'
#' @examples
check_stand <- function(a) {

  # First extract species of adult trees present in all plots.
  b <- a %>% pull(trees)
  adultsp <- unique(unlist(sapply(b, function(x) switch(stand_type,
                                                             individual = unique(x$species),
                                                             ipm = colnames(x))
  )))

  # Same for smaller trees.
  b <- a %>% pull(saplings)
  saplingsp <- unique(unlist(sapply(b, function(x) unique(x$species))))

  return(list(species.adults = adults,
              species.saplingsp = saplingsp))
}
