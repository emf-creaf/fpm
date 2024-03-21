test_that("Set attributes of sf tree stand object", {

  # Create sf stand.
  a <- start_stands()

  # Default value.
  expect_identical(attr(a, "country"), "spain")

  # Wrong country.
  expect_error(set_attributes(a, "italy"))

  # country is not set here..
  expect_warning(a <- set_parameters(a, param = list(country = "spain", crs = "ASDF")))

  # Species integvars and h.
  x <- list('Pinus halepensis' = seq(7.5, 200, length = 1000),
            'Quercus ilex' = seq(7.5, 170, length = 1500))
  a <- set_parameters(a, param = list(integvars = x))

  # Nothing is done.
  expect_warning(set_parameters(a))

  # "country" is not evaluated.
  expect_warning(set_parameters(a, param = list(country = "australia")))

  # Wrong integvars.
  expect_error(set_parameters(a, country = "spain", integvars = 1:3))

  # Correct integvars.
  expect_no_error(set_parameters(a, param = list(integvars = list(x=1:3, y=(4:6)/2))))

  # Both approaches are equivalent.
  mindbh <- list('Pinus halepensis' = 7.5, 'Quercus ilex' = 7.5)
  maxdbh <- list('Pinus halepensis' = 200, 'Quercus ilex' = 170)
  crs = "EPSG:4326"
  a <- start_stands()
  a <- set_parameters(a, param = list(integvars = x,
                      mindbh = mindbh,
                      maxdbh = maxdbh,
                      crs = crs))
  b <- start_stands(param = list(country = "spain",
                                   integvars = x,
                                   mindbh = mindbh,
                                   maxdbh = maxdbh,
                                   crs = crs))
  expect_identical(a, b)

})
