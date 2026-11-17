#' Title
#'
#' @param integvars named \code{list} with vector of abscissas per species.
#' @param h named \code{list} with integration intervals per species.
#' @param mindbh named \code{list} with minimum DBH per species.
#' @param maxdbh named \code{list} with maximum DBH per species.
#' @param tolerance \code{numeric} ≥ 0. Differences smaller than tolerance are not reported.
#' The default value is sqrt(.Machine$double.eps).
#'
#' @returns
#' An invisible TRUE value.
#'
#' @export
#'
#' @examples
#' names <- c('Pinus nigra', 'Quercus ilex', 'Fagus sylvatica')
#' h <- setNames(list(.1, .025, .03), names)
#' mindbh <- setNames(list(.1, .025, .03), names)
#' maxdbh <- setNames(list(150, 250, 190), names)
#' x <- lapply(names, function(x) seq(mindbh[[x]], maxdbh[[x]], by = h[[x]]))
#' x <- setNames(x, names)
#' check_abscissas(x, h, mindbh, maxdbh)
#'
check_abscissas <- function(integvars  = NULL, h = NULL, mindbh = NULL, maxdbh = NULL, tolerance = NULL) {

  # First checks.
  if (any(is.null(mindbh), is.null(maxdbh), is.null(integvars), is.null(h))) {
    cli::cli_abort("Parameters 'mindbh', 'maxdbh', 'integvars' or 'h' have not been provided")
  }

  if (!is.list(mindbh) || !is.list(maxdbh) || !is.list(integvars) || !is.list(h)) {
    cli::cli_abort("Parameters 'mindbh', 'maxdbh', 'integvars' and 'h' must be 'list' objects")
  }

  if (is.null(tolerance)) {
    tolerance = sqrt(.Machine$double.eps)
  } else {
    if (tolerance < 0) cli::cli_abort("Parameter 'tolerance' cannot be negative")
  }


  # Check that element names match.
  sp_mindbh <- sort(names(mindbh))
  sp_maxdbh <- sort(names(maxdbh))
  sp_integvars <- sort(names(integvars))
  sp_h <- sort(names(h))

  if (!DescTools::AllIdentical(sp_mindbh, sp_maxdbh, sp_integvars, sp_h)) {
    cli::cli_abort("Species names in lists 'mindbh', 'maxdbh', 'integvars' or 'h' do not match")
  }


  # We prepare sorted versions of the inputs.
  mindbh <- mindbh[sp_mindbh]
  maxdbh <- maxdbh[sp_maxdbh]
  h <- h[sp_mindbh]
  integvars <- integvars[sp_integvars]


  # Check that h, mindbh and maxdbh are coherent, i.e. h > 0 and 0 < mindbh < maxdbh always.
  if (!all(h > 0) || !all(mindbh >= 0) || !all(maxdbh >= 0)) {
    cli::cli_abort("Parameters 'mindbh', 'maxdbh' and 'h' must be always positive")
  }

  if (any(unlist(mindbh) >= unlist(maxdbh))) {
    cli::cli_abort("mindbh must be < maxdbh")
  }


  # 'h' increment must be constant.
  h2 <- lapply(integvars, function(x) diff(x))
  test_h <- sapply(names(h), function(x) all(abs(h[[x]] - h2[[x]]) <= tolerance))
  if (!all(test_h)) {
    notTRUE <- !test_h
    cli::cli_abort(paste0("Increment 'h' is not consistently constant across 'integvars' abscissaes for species: ",
                          paste0(names(h)[notTRUE], collapse = ", ")))
  }


  # Minimum, maximum and h must be consistent too.
  test_consistency <- lapply(names(h), function(x) {
    all.equal(integvars[[x]], seq(mindbh[[x]], maxdbh[[x]], by = h[[x]]))
  })
  if (!all(unlist(test_consistency))) {
    cli::cli_abort("Cannot reproduce 'integvars' abscissas with parameters 'h', 'mindbh', 'maxdbh'")
  }


  invisible(TRUE)

}
