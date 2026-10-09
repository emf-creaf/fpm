test_that("Checking list of models", {

  l <- 3
  expect_error(check_list_of_models(l))

  l <- list(a = 4, b = "e")
  expect_error(check_list_of_models(l))

  l <- list(
    "seedlings" = 4,
    "saplings" = "e",
    "ingrowth" = list(a = 3),
    "lambda" = runif(10),
    "survival" = letters,
    "growth" = data.frame()
  )
  expect_error(check_list_of_models(l))

  x <- 1:100
  set.seed(100)
  y <- rnorm(100)
  r <- lm(y ~ x)
  l <- list(
    "seedlings" = r,
    "saplings" = "e",
    "ingrowth" = list(a = 3),
    "lambda" = runif(10),
    "survival" = letters,
    "growth" = data.frame(),
    "variance" = r
  )
  expect_error(check_list_of_models(l))

  l <- list(
    "seedlings" = r,
    "saplings" = r,
    "ingrowth" = r,
    "lambda" = r,
    "survival" = r,
    "growth" = r,
    "variance" = r
  )
  expect_no_error(check_list_of_models(l))

})
