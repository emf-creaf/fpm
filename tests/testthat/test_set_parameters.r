test_that("Set attributes of sf tree stand object", {

  # Create sf stand.
  a <- start_stands()

  # Wrong country.
  expect_error(set_attributes(a, "italy"))

  # Right country.
  a <- set_parameters(a, country = "spain")
  expect_identical(attr(a, "country"), "spain")

  # Species integvars and h.
  x <- list('Pinus halepensis' = seq(7.5, 200, length = 1000),
            'Quercus ilex' = seq(7.5, 170, length = 1500))
  a <- set_parameters(a, integvars = x)

  # Nothing is done
  expect_warning(set_parameters(a))

  # Wrong integvars.
  expect_error(set_parameters(a, country = "spain", integvars = 1:3))

  # Correct integvars.
  expect_no_error(set_parameters(a, country = "spain", integvars = list(x=1:3, y=4:6)))

})
