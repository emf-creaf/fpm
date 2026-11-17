# Constructor
new_stands_sf <- function(sf_plots = NULL, stands_data = NULL) {

  # Checks.
  if (is.null(sf_plots) || !inherits(sf_plots, "sf")) {
    cli::cli_abort("Input 'sf_plots' must be a 'sf' object")
  }

  if (is.null(stands_data)) {
    stands_data = list()
  }

  if (!inherits(x, "sf")) {
    cli::cli_abort("Input 'x' must be a 'sf' object")
  }

  if (!inherits(stands_data, "list")) {
    cli::cli_abort("Input 'stands_data' must be a list")
  }


  x = sf::st_sf(sf::st_sfc(x))

  # Add attributes and build a new class. Correctness of parameters is not checked.
  structure(
    x,
    stands_data = stands_data,
    class = unique(c("stands_sf", class(x)))
  )

}

# Helper to restore stands_sf class and attributes
restore_stands_sf <- function(out, original) {
  if (inherits(out, "sf")) {
    new_stands_sf(out, stands_data = attr(original, "stands_data"))
  } else {
    out
  }
}

# 1. Standard dplyr hooks
#' @exportS3Method dplyr::dplyr_reconstruct stands_sf
dplyr_reconstruct.stands_sf <- function(data, template) {
  out <- NextMethod()
  restore_stands_sf(out, template)
}

#' @exportS3Method dplyr::dplyr_col_modify stands_sf
dplyr_col_modify.stands_sf <- function(.data, cols) {
  out <- NextMethod()
  restore_stands_sf(out, .data)
}

# 2. sf-specific dplyr verb overrides (where sf bypasses dplyr_reconstruct)
#' @export
select.stands_sf <- function(.data, ...) {
  out <- NextMethod()
  restore_stands_sf(out, .data)
}

#' @export
filter.stands_sf <- function(.data, ..., .by = NULL) {
  out <- NextMethod()
  restore_stands_sf(out, .data)
}

#' @export
mutate.stands_sf <- function(.data, ..., .by = NULL) {
  out <- NextMethod()
  restore_stands_sf(out, .data)
}

#' @export
arrange.stands_sf <- function(.data, ..., .by_group = FALSE) {
  out <- NextMethod()
  restore_stands_sf(out, .data)
}

#' @export
slice.stands_sf <- function(.data, ..., .by = NULL) {
  out <- NextMethod()
  restore_stands_sf(out, .data)
}
