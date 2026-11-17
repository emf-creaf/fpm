#' Title
#'
#' @param sf_plots
#' @param type
#' @param df
#' @param lst_models
#' @param verbose
#'
#' @returns
#' @export
#'
#' @examples
check_param_fpm <- function(sf_plots = NULL, type = NULL, df = NULL, lst_models = NULL) {

  # Check 'sf_plots' is not NULL and it is a 'sf' object.
  if (is.null(sf_plots) || !inherits(sf_plots, "sf")) {
    cli::cli_abort("Parameter 'sf_plots' must be a 'sf' object")
  }

  # Check 'type' is ok.
  if (!is.null(type) || !is.character(type) || length(type) != 1) {
    cli::cli_abort("Wrong 'type' value")
  }

  if (!any(type %in% c("seedlings", "saplings", "ingrowth"))) {
    cli::cli_abort("Parameter 'type' must be equal to 'seedlings'', 'saplings' or 'ingrowth'")
  }

  # Check 'df' is a data.frame.
  if (!is.null(df) || !is.data.frame(df)) {
    cli::cli_abort("Wrong 'df' parameter")
  }

  # Check idplot identifier in 'sf_plots' and 'df' match.
  if (!identical(sf_plots$idplot, df$idplot)) cli::cli_abort("Index 'idplot' in 'sf_plots' and 'df' do not match exactly")


  # Check attributes in sf_plots.
  check_attributes()


  # Check list of models.
  check_list_of_models(lst_models)

  invisible(TRUE)

}
