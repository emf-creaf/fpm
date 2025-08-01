test_that("Numerical integration works", {

  h <- .1
  x <- seq(0, 5, by = h)
  y <- dnorm(x)

  # Wrong inputs.
  expect_error(quadrature(list(x), h = h))
  expect_error(quadrature(letters, h = h))
  expect_error(quadrature(y, h = -.1))
  expect_error(quadrature(y[1], h = h))
  expect_error(quadrature(y[1:8], h = h, type = "simpson"))
  expect_error(quadrature(y, h = h, type = "dummy"))

  # Vector.
  expect_lt(abs(integrate(dnorm, 0, 5)$value - quadrature(y, h = h)), 1e-5)
  expect_lt(abs(integrate(dnorm, 0, 5)$value - quadrature(y, h = h, type = "simpson")), 1e-5)

  # Matrix.
  x <- x %*% t(rep(1, 100))
  expect_lt(max(abs(integrate(dnorm, 0, 5)$value - quadrature(y, h = h))), 1e-5)
  expect_lt(max(abs(integrate(dnorm, 0, 5)$value - quadrature(y, h = h, type = "simpson"))), 1e-5)

})
