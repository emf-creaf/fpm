#' Check data.frame containing tree data
#'
#' @param tree_data a \code{data.frame} with seedings, saplings or tree data.
#' @param verbose \code{logical}, if set to TRUE info messages are printed on screen.
#'
#' @returns
#' An invisible TRUE if everything is correct.
#'
#' @details
#' The input 'tree_data' data.frame is checked for its columns and the nested data.frames
#' containing data for seedlings, saplings or trees.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' df <- data.frame(idplot = 1:2)
#' # Error.
#' check_input_tree_data(df)
#' }
#'
check_input_tree_data <- function(tree_data = NULL, verbose = TRUE) {


  if (verbose) {
    fn_name <- as.character(sys.call()[[1]])
    cat("\n")
  }


  # Minimum requirement for 'tree_data'.
  if (verbose) {
    cli::cli_alert_info("[{fn_name}] Checking input is a data.frame")
  }
  if (!is.data.frame(tree_data)) {
    cli::cli_abort("Input 'tree_data' must be a data.frame object")
  }


  # Check that 'idplot' is present.
  if (verbose) {
    cli::cli_alert_info("[{fn_name}] Checking 'input'idplot' is present")
  }
  if (!("idplot" %in% colnames(tree_data))) {
    cli::cli_abort("Input data.frame 'tree_data' must contain a column labelled 'idplot'")
  }


  # 'idplot' must be of character type.
  if (verbose) {
    cli::cli_alert_info("[{fn_name}] Checking 'idplot' is of character type")
  }
  if (!is.character(tree_data$idplot)) {
    cli::cli_abort("Column 'idplot' in data.frame 'tree_plot' must be of character type")
  }


  # Those indices must be unique, i.e. one per row.
  if (verbose) {
    cli::cli_alert_info("[{fn_name}] Checking 'idplot' is unique for each row")
  }
  nr <- length(tree_data$idplot)
  if (nr != length(unique(tree_data$idplot))) {
    cli::cli_abort("Index in 'idplot' column must be unique for each row")
  }


  # Check that coordinates are also available.
  if (verbose) {
    cli::cli_alert_info("[{fn_name}] Checking coordinates 'x', 'y' are present")
  }
  if (!all(c("x", "y") %in% colnames(tree_data))) {
    cli::cli_abort("Input coordinates must be present as columns 'x' and 'y' in 'tree_data'")
  }


  # Check that coordinates are numeric.
  if (verbose) {
    cli::cli_alert_info("[{fn_name}] Checking coordinates 'x', 'y' are numeric")
  }
  if (!all(is.numeric(tree_data$x) && is.numeric(tree_data$y))) {
    cli::cli_abort("Input coordinates 'x' and 'y' must be of numeric type")
  }


  # Check that there is at least one element labelled seedlings, saplings or trees.
  if (verbose) {
    cli::cli_alert_info("[{fn_name}] Checking there is at least one column labelled 'seedlings', 'saplings' or 'trees'")
  }
  if (!any(c("seedlings", "saplings", "trees") %in% colnames(tree_data))) {
    cli::cli_abort("Input data.frame 'tree_data' must contain at least one column labelled 'seedlings', 'saplings' or 'trees'")
  }


  # Check that nested components are data.frame that they have the right columns,
  # that those columns are of the right type and that they are not empty.
  check_column_names_format <- function(colnam) {

    if (colnam %in% colnames(tree_data)){

      # Nested components are data.frames.
      if (verbose) {
        cli::cli_alert_info("[{fn_name}] Checking that '{colnam}' contain data.frames in each row")
      }
      if (!all(sapply(tree_data[[colnam]], is.data.frame))) {
        cli::cli_abort("'{colnam}' must contain data.frames in each row")
      }

      # Not empty.
      if (verbose) {
        cli::cli_alert_info("[{fn_name}] Checking that data.frames in '{colnam}' are not empty")
      }
      x <- sapply(tree_data[[colnam]], ncol)
      if (all(x == 0)) {
        cli::cli_abort("data.frames '{colnam}' cannot be empty")
      }

      # Column names are correct.
      if (verbose) {
        cli::cli_alert_info("[{fn_name}] Checking that column names of data.frames in '{colnam}' are correct")
      }
      firstcol <- ifelse(colnam == "trees", "dbh", "n")
      x <- sapply(tree_data[[colnam]], function(s) identical(c(firstcol, "species"), sort(colnames(s))))
      if (!all(x)) {
        cli::cli_abort("data.frames '{colnam}' must have two columns '{firstcol}' and 'species'")
      }

      # They have the right type.
      if (verbose) {
        cli::cli_alert_info("[{fn_name}] Checking that columns of data.frames in '{colnam}' have the right type")
      }
      x <- sapply(tree_data[[colnam]], function(s) is.numeric(s[[firstcol]]) && is.character(s[["species"]]))
      if (!all(x)) {
        cli::cli_abort("Wrong column types in '{colnam}'")
      }

      # Strings in 'species' are not empty.
      if (verbose) {
        cli::cli_alert_info("[{fn_name}] Checking that columns 'species' does not have empty strings")
      }
      x <- sapply(tree_data[[colnam]], function(s) nchar(s[["species"]]) > 0)
      if (!all(x)) {
        cli::cli_abort("Column 'species' in '{colnam}' cannot have empty strings")
      }


      # Values in 'dbh' or n' are not negative.
      if (verbose) {
        cli::cli_alert_info("[{fn_name}] Checking '{firstcol}' in '{colnam}' is >= 0")
      }
      x <- sapply(tree_data[[colnam]], function(s) all(s[[firstcol]] >= 0))
      if (!all(x)) {
        cli::cli_abort("Column '{firstcol}' in '{colnam}' must be >= 0")
      }

    }
  }

  check_column_names_format("seedlings")
  check_column_names_format("saplings")
  check_column_names_format("trees")


  invisible(TRUE)
}
