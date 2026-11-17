test_that("integvars list is ok", {

  names <- c('Pinus nigra', 'Quercus ilex', 'Fagus sylvatica')
  h <- setNames(list(.1, .025, .03), names)
  mindbh <- setNames(list(.1, .025, .03), names)
  maxdbh <- setNames(list(150, 250, 190), names)
  x <- setNames(list(seq(7.5, 150, .1), 1:200, 1:125), names)

  # NULL inputs
  expect_error(check_abscissas())

  # Not-a-list inputs.
  expect_error(check_abscissas(1:199, 1, 1, 3))

  # Always mindbh < maxdbh
  expect_error(check_abscissas(x, h, setNames(list(1, 1, 3000), names), maxdbh, -1))

  # Tolerance cannot be negative.
  expect_error(check_abscissas(x, h, mindbh, maxdbh, -1))

  # Wrong species names.
  expect_error(check_abscissas(setNames(x, names[c(1, 1, 3)]), h, mindbh, maxdbh))
  expect_error(check_abscissas(x, setNames(h, names[c(1, 1, 3)]), mindbh, maxdbh))
  expect_error(check_abscissas(x, h, setNames(mindbh, names[c(1, 1, 3)]), maxdbh))
  expect_error(check_abscissas(x, h, mindbh, setNames(maxdbh, names[c(1, 1, 3)])))

  # Increment 'h' must be constant.
  x <- setNames(list(sort(runif(100)), sort(runif(150)), sort(runif(25))), names)
  expect_error(check_abscissas(x, h, mindbh, maxdbh))

  # Minimum, maximum and h must be consistent.
  x <- setNames(list(seq(7.5, 150, .1), 1:200, 1:125), names)
  expect_error(check_abscissas(x, h, mindbh, maxdbh))

  # Input is fine.
  x <- lapply(names, function(x) {
    seq(mindbh[[x]], maxdbh[[x]], by = h[[x]])
  })
  x <- setNames(x, names)
  expect_true(check_abscissas(x, h, mindbh, maxdbh))

})
