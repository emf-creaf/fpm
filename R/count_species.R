count_species <- function(a, verbose = T) {

  # Must be an "sf" object.
  stopifnot("Input 'a' must be an 'sf' object" = any(class(a) == "sf"))


  # Check country.
  country <- match.arg(attr(a, "country"), c("spain", "france", "usa"))


  # If progress is TRUE, print a progress bar.
  if (verbose) {
    cat("\n -> count_species: Counting all species...\n")
    pb <- txtProgressBar(min = 0,
                         max = nrow(a),
                         style = 3,
                         width = 50,
                         char = "=")
  }


  species <- NULL
  icount <- 0
  for (i in 1:nrow(a)) {

    # Progress bar.
    icount <- icount + 1
    if (verbose) setTxtProgressBar(pb, icount)


    # Calculate only if there are trees.
    b <- a[i, ]
    if (length(b) > 0) {
      if (b$stand_type == "individual") {
        if (country == "spain") {
          sp <- unique(b$trees[[1]]$species)
        }
      } else if (b$stand_type == "ipm") {
        sp <- names(b$trees[[1]])
      }
      if (length(sp)>0) {
        species <- c(species, sp)
      }
    }
  }

  if (verbose) cat("\n")

  return(unique(species))
}
