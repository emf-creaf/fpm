test_that("Check 'integvars' works", {

  #
  mindbh <- c('Pinus nigra' = 7.5, 'Pinus pinea' = 7.5)
  maxdbh <- c('Pinus nigra' = 200, 'Pinus pinea' = 220)
  x <- integvars(mindbh, maxdbh, by = 10)

  expect_true(class(x) == "list")
  expect_true(length(x) == 2)
  expect_identical(names(x), c("Pinus nigra", "Pinus pinea"))
  expect_identical(sapply(x, length), c('Pinus nigra' = 20L, 'Pinus pinea' = 22L))

  # Names do not match.
  mindbh <- c('Quercus ilex' = 7.5, 'Pinus pinea' = 7.5)
  expect_error(integvars(mindbh, maxdbh, by = 10))



})
