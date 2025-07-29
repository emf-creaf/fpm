test_that("Check 'integvars' works", {

  #
  mindbh <- c('Pinus nigra' = 7.5, 'Pinus pinea' = 7.5)
  maxdbh <- c('Pinus nigra' = 200, 'Pinus pinea' = 220)
  x <- integvars(mindbh, maxdbh, by = 10)

  expect_true(class(x) == "list")
  expect_true(length(x) == 2)
  expect_identical(names(x), c("Pinus nigra", "Pinus pinea"))
  expect_identical(sapply(x, length), c('Pinus nigra' = 20L, 'Pinus pinea' = 22L))
  expect_identical(sapply(sapply(x, diff), mean), c('Pinus nigra' = 10, 'Pinus pinea' = 10))

  # Names do not match.
  mindbh <- c('Quercus ilex' = 7.5, 'Pinus pinea' = 7.5)
  expect_error(integvars(mindbh, maxdbh, by = 10))

  # Different increment per species.
  mindbh <- c('Pinus nigra' = 7.5, 'Pinus pinea' = 7.5)
  x <- integvars(mindbh, maxdbh, by = c('Pinus nigra' = .5, 'Pinus pinea' = .22))
  expect_equal(sapply(x, length), c('Pinus nigra' = 386, 'Pinus pinea' = 966))
  expect_identical(sapply(sapply(x, diff), mean), c('Pinus nigra' = 0.50, 'Pinus pinea' = 0.22))


  # In case a given length of exactly 100 is required.
  length <- 100
  by <- (maxdbh-mindbh)/(length-1)
  x <- integvars(mindbh, maxdbh, by)
  expect_equal(sapply(x, length), c('Pinus nigra' = 100, 'Pinus pinea' = 100))
  expect_true(all(abs(apply(sapply(x, diff), 2, mean) - c('Pinus nigra' = 1.94444, 'Pinus pinea' = 2.146465)) < 1e-05))

})
