#' Calculate descriptive statistics of \code{sf} stands object.
#'
#' @description
#' New fields are added to the \code{stands} \code{sf} object containing plot statistics.
#'
#' @param a a \code{sf} object containing a number of POINT geometry types.
#' @param verbose
#'
#' @return
#' The same \code{sf} input object with new fields containing plot statistics.
#'
#' @details
#' Fields are added to the \code{a} object containing the basal area per species (\code{ba_species}),
#' total basal area (\code{ba}), number of trees per species (\code{ntrees_species})
#' and total number of trees (\code{ntrees}).
#'
#' @export
#'
#' @examples
#' #' a <- start_stands()
#' max_dbh <- list('Pinus halepensis' = 200, 'Pinus nigra' = 230)
#' a <- set_parameters(a, param = list(max_dbh = max_dbh, crs =  "EPSG:4326"))
#'
#' # Next, we add one stand.
#' df <- data.frame(species = c('Pinus halepensis', 'Quercus ilex'), dbh = c(8.6, 12.7))
#' a <- build_stand(a, "id1", data = list(df = df), verbose = T)
#'
#' # Add fields with statistics.
#' a <- get_stats(a)
#'
get_stats <- function(a, verbose = T) {


  # Must be an "sf" object.
  stopifnot("Input 'a' must be an 'sf' object" = inherits(a, "sf"))


  # We may (or may not) need this below.
  xabs <- get_parameters(a, "integvars")[[1]]
  h <- get_parameters(a, "h")[[1]]


  # If verbose is TRUE, print a progress bar.
  if (verbose) {
    fname <- as.character(match.call()[[1]])
    cat(paste0("\n -> ", fname, ": Creating new 'sf' with stats per plot...\n"))
    pb <- utils::txtProgressBar(min = 0,
                                max = length(a$idplot),
                                style = 3,
                                width = 50,
                                char = "=")
  }


  # Add new fields to 'a'.
  a$ba_species <- vector("list", length(a$idplot))
  a$ba <- -1
  a$ntrees_species <- vector("list", length(a$idplot))
  a$ntrees <- -1


  # Go plot by plot.
  icount = 0
  for (i in 1:length(a$idplot)) {

    # Progress bar.
    icount <- icount + 1
    if (verbose) setTxtProgressBar(pb, icount)

    b <- a[i, ]

    # Calculates basal areas for trees, if there is any.
    if (length(b$trees) > 0) {
      if (b$stand_type == "ipm") stopifnot("Parameter 'integvars' has not been specified" = !is.null(xabs))
      a$ba_species[[i]] <- calc_ba(b, xabs, h)
      a$ntrees_species[[i]] <- calc_ntrees(b, xabs, h)
      a$ba[i] <- sum(unlist(a$ba_species[[i]]$ba))
      a$ntrees[i] <- sum(unlist(a$ntrees_species[[i]]$ntrees))
    }
  }


  # Extra carriage return.
  if (verbose) cat("\n")

  return(a)

}
