#' Title
#'
#' @param young
#' @param ingrowth
#' @param trees
#' @param verbose
#'
#' @return
#' @export
#'
#' @examples
collect_parts <- function(seedling, sapling, ingrowth, adult, verbose = T) {

  # First checks.
  stopifnot("Input 'seedling' must be an 'sf' object" = inherits(seedling, "sf"))
  stopifnot("Input 'sapling' must be an 'sf' object" = inherits(sapling, "sf"))
  stopifnot("Input 'ingrowth' must be an 'sf' object" = inherits(ingrowth, "sf"))
  stopifnot("Input 'adult' must be an 'sf' object" = inherits(adult, "sf"))


  # Plots must match.
  stopifnot("Plot identifiers do not match" =
              (identical(seedling$idplot, adult$idplot) &
                 identical(sapling$idplot, adult$idplot) &
                 identical(ingrowth$idplot, adult$idplot)))


  # Countries must match.
  country <- c(match.arg(tolower(attr(seedling, "country")), c("spain", "france", "usa")),
               match.arg(tolower(attr(sapling, "country")), c("spain", "france", "usa")),
               match.arg(tolower(attr(ingrowth, "country")), c("spain", "france", "usa")),
               match.arg(tolower(attr(adult, "country")), c("spain", "france", "usa")))
  stopifnot("'country' parameters do not match" = length(unique(country)) == 1)
  country <- country[1]


  # If verbose is TRUE, print a progress bar.
  if (verbose) {
    fname <- as.character(match.call()[[1]])
    cat(paste0("\n -> ", fname, ": Collecting seedling, sapling, ingrowth and tree data...\n"))
    pb <- utils::txtProgressBar(min = 0,
                                max = length(a$idplot),
                                style = 3,
                                width = 50,
                                char = "=")
  }


  # New stand 'sf' object.
  b <- clear_stands(seedling)


  # Petit function to be used below.
  f <- function(z) if (is.null(z)) 0 else z

  icount = 0
  for (i in 1:nrow(b)) {

    # Progress bar.
    icount <- icount + 1
    if (verbose) setTxtProgressBar(pb, icount)


    # Filling in young trees.
    b$seedlings[[i]] <- seedling$seedlings[[i]]
    b$saplings[[i]] <- sapling$saplings[[i]]


    # Read ingrowth data.
    x <- ingrowth$trees[[i]]
    species_ingrowth <- names(x)


    # Read adult trees data
    y <- adult$trees[[i]]
    species_trees <- names(y)


    # Species present in the plot.
    species_unique <- unique(c(species_ingrowth, species_trees))


    # Adding adults.
    if (length(species_unique) > 0) {
      for (j in species_unique) b$trees[[i]][[j]] <- f(x[[j]]) + f(y[[j]])
    }

  }

  if (verbose) cat("\n")


  return(b)
}
