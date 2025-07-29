test_that("Convolving with Gaussian or bounded uniform distributions", {

  trap <- function(y, h) (sum(y) - (y[1]+y[length(y)])/2)*h

  # Define points to be smoothed. Reduce h for better approximation.
  h <- 0.01
  x <- seq(7.5, 50, by = h)
  y <- c(7, 8, 8.5, 9, 29, 49, 49.5, 50)

  # First we test functions separately.
  expect_error(convolve_gaussian(list(x), y, width = 5, normalization = FALSE))
  expect_error(convolve_gaussian(x, list(y), width = 5, normalization = FALSE))
  expect_error(convolve_uniform(list(x), y, width = 5, normalization = FALSE))
  expect_error(convolve_uniform(x, list(y), width = 5, normalization = FALSE))

  # Total number of points for normalized and not normalized.
  z1 <- convolve_gaussian(x, y, width = 5, normalization = FALSE)
  z1norm <- convolve_gaussian(x, y, width = 5)

  z2 <- convolve_uniform(x, y, width = 5, normalization = FALSE)
  z2norm <- convolve_uniform(x, y, width = 5)

  expect_true(abs(trap(z1, h) - 8)/8 > .3)
  expect_true(abs(trap(z1norm, h) - 8)/8 < .0001)
  expect_true(abs(trap(z2, h) - 8)/8 > .3)
  expect_true(abs(trap(z2norm, h) - 8)/8 < .01)

  # Next we test kernsmooth.
  expect_error(kernsmooth(list(x), y, type = "gaussian", width = 5, normalization = FALSE))
  expect_error(kernsmooth(x, list(y), type = "uniform", width = 5, normalization = FALSE))
  expect_error(kernsmooth(x, y, type = "dummy", width = 5, normalization = FALSE))

  # Total number of points for normalized and not normalized.
  z1 <- kernsmooth(x, y, type = "gaussian", width = 5, normalization = FALSE)
  z1norm <- kernsmooth(x, y, type = "gaussian", width = 5)

  z2 <- kernsmooth(x, y, width = 5, type = "uniform", normalization = FALSE)
  z2norm <- kernsmooth(x, y, type = "uniform", width = 5)

  expect_true(abs(trap(z1, h) - 8)/8 > .3)
  expect_true(abs(trap(z1norm, h) - 8)/8 < .0001)
  expect_true(abs(trap(z2, h) - 8)/8 > .3)
  expect_true(abs(trap(z2norm, h) - 8)/8 < .01)

})
