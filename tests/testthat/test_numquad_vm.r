test_that("Numerical quadrature of vector times matrix", {

  # Error: vector is a matrix.
  expect_error(numquad_vm(matrix(runif(10),5,2), matrix(runif(10),5,2), 4, 1))

  # Error: matrix is a vector.
  expect_error(numquad_vm(1:5, 1:7, 4, 1))

  # Error: length of vector v is different from nv.
  expect_error(numquad_vm(1:5, matrix(runif(10),5,2), 4, 1))

  # Error: length of vector v and number of rows of m do not match.
  expect_error(numquad_vm(1:5, matrix(runif(10),2,5), 5, 1))

  # Error: nv/h is a vector, not a number.
  expect_error(numquad_vm(1:5, matrix(runif(10),5,2), c(5,5), 1))
  expect_error(numquad_vm(1:5, matrix(runif(10),5,2), 5, c(1,2)))

  # There are no differences with MiscMath::quad_trapez or MiscMath::quad_ext_simpson
  nv <- 500
  nc <- 100
  v <- seq(0, 2*pi, length = nv)
  m <- matrix(nv*nc, nv, nc)
  for (i in 0:nc) m[, i] <- sin((v-i/nc*pi))
  h <- v[2]-v[1]
  x1 <- numquad_vm(v, m, nv, h)
  y1 <- sapply(1:nc, function(i) MiscMath::quad_trapez(v*m[,i], h))
  x2 <- numquad_vm(v, m, nv, h, "simpson")
  y2 <- sapply(1:nc, function(i) MiscMath::quad_ext_simpson(v*m[,i], h))
  true_value <- sapply(1:nc, function(i) {
    vmax <- v[nv]-i/nc*pi
    vmin <- v[1]-i/nc*pi
    (sin(vmax)-vmax*cos(vmax))-(sin(vmin)-vmin*cos(vmin))
  })

  # Differences between equivalent algorithms are negligible.
  expect_lt(mad(x1-y1), 1e-12)
  expect_lt(mad(x2-y2), 1e-12)

  # Differences with true_value are also very small.
  expect_lt(mad(x1-true_value), 1e-4)
  expect_lt(mad(x2-true_value), 1e-6)

})
