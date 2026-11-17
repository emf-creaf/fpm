expect_error_exact <- function(object, message) {

  old_opts <- options(width = 10000L, cli.width = 10000L)
  on.exit(options(old_opts), add = TRUE)

  err <- tryCatch(
    force(object),
    error = identity
  )

  testthat::expect_true(
    inherits(err, "error"),
    info = "Expected an error, but no error was thrown."
  )


  testthat::expect_identical(
    cli::ansi_strip(conditionMessage(err)),
    message
  )

  invisible(err)
}


