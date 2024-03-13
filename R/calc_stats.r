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
#' a <- start_stands()
#' max_dbh <- list('Pinus halepensis' = 200, 'Pinus nigra' = 230)
#' a <- set_parameters(a, param = list(max_dbh = max_dbh, crs =  "EPSG:4326"))
#'
#' # Next, we add one stand.
#' df <- data.frame(species = c('Pinus halepensis', 'Quercus ilex'), dbh = c(8.6, 12.7))
#' a <- build_stands(a, "id1", data = list(df = df), verbose = T)
#'
#' # Add fields with statistics.
#' a <- calc_stats(a)
#'
calc_stats <- function(a, verbose = T) {


  # Must be an "sf" object.
  stopifnot("Input 'a' must be an 'sf' object" = inherits(a, "sf"))


  # Retrieve parameters.
  p <- a |> get_parameters(c("country", "integvars", "h"))
  country <- p$country


  # Only country == "spain" has been implemented.
  if (country == "usa") {
    stop("Calculations for country = 'usa' have not yet been implemented")
  } else if (country == "france") {
    stop("Calculations for country = 'france' have not yet been implemented")
  }


  # If verbose is TRUE, print a progress bar.
  if (verbose) {
    fname <- as.character(match.call()[[1]])
    cat(paste0("\n -> ", fname, ": Creating new 'sf' with stats per plot...\n"))
    pb <- utils::txtProgressBar(min = 0,
                                max = nrow(a),
                                style = 3,
                                width = 50,
                                char = "=")
  }


  # Add new fields to 'a'.
  a$ba_species <- a$ntrees_species <- vector("list", length(a$idplot))
  a$ba <- a$ntrees <- 0


  # Go plot by plot.
  icount = 0
  for (i in 1:nrow(a)) {

    # Progress bar.
    icount <- icount + 1
    if (verbose) setTxtProgressBar(pb, icount)


    # Calculations.
    a[i, ] <- calc_descriptive(a[i, ], param = list(integvars = p$integvars, h = p$h, country = p$country))

  }


  # Extra carriage return.
  if (verbose) cat("\n")

  return(a)

}
