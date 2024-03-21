#' Numerical quadrature of the IPM integral.
#'
#' @param a a \code{sf} object containing a number of POINT geometry types.
#' @param data \code{list} containing the survival (vector) and growth (matrix) components
#' for the IPM quadrature.
#' @param verbose logical, if set to TRUE a progress bar will be printed.
#'
#' @return
#' A \code{sf} object with the projected tree population.
#' @export
#'
#' @examples
#' See Vignettes.
fpm_quadrature <- function(a, data = list(), verbose = T, method = "trapez") {


  # Check quadrature method.
  method <- match.arg(tolower(method), c("trapez", "simpson"))


  # Retrieve parameters.
  p <- get_parameters(a, c("country", "h"))
  country <- p$country
  h <- p$h


  # Retrieve fpm elements.
  survival <- data$survival
  growth <- data$growth


  # First initialize stands.
  b <- clear_stands(a)


  # If verbose is TRUE, print a progress bar.
  if (verbose) {
    fname <- as.character(match.call()[[1]])
    cat(paste0("\n -> ", fname, ": Applying quadrature to solve integral numerically...\n"))
    pb <- utils::txtProgressBar(min = 0,
                                max = length(a$idplot),
                                style = 3,
                                width = 50,
                                char = "=")
  }


  # Loop.
  icount = 0
  for (i in 1:nrow(a)) {


    # Progress bar.
    icount <- icount + 1
    if (verbose) setTxtProgressBar(pb, icount)


    # Continue only if "spain".
    if (country == "spain") {

      # Continue only if "ipm".
      if (a[i, ]$stand_type == "ipm") {

        # Continue only if there are trees to project.
        if (length(a[i, ]$trees[[1]]) > 0) {

          # Numerical quadrature with extended Simpson' rule.
          for (j in names(a[i, ]$trees[[1]])) {
            su <- survival[i, ]$trees[[1]][[j]]
            gr <- growth[i, ]$trees[[1]][[j]]
            n1 <- a[i, ]$trees[[1]][[j]]
            b[i, ]$trees[[1]][[j]] <- numquad_vm(n1 * su, gr, h[[j]], method)
          }
        }
      }
    }
  }

  if (verbose) cat("\n")

  return(b)

}
