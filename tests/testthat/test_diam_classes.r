test_that("Numerical quadrature of a tabulated function", {

  # Abscissas.
  x <- seq(-3, 60, by=.17)
  xl <- seq(0, 54, length = 20)

  # Functions to integrate.
  y1 <- sin(x*pi/54)
  y2 <- exp(-.15*x)

  # Check inputs.
  expect_error(diam_classes(x, y1))

  # Length of output should be length(xl)-1.
  expect_equal(length(diam_classes(x, y1, xl)), length(xl)-1)

  # Approximation to actual integrals.
  expect_lt(abs(sum(diam_classes(x, y1, xl))-(1-cos(pi))*54/pi), .015)
  expect_lt(abs(sum(diam_classes(x, y2, xl))-(exp(-.15*min(xl))-exp(-.15*max(xl)))/.15), .015)

})
