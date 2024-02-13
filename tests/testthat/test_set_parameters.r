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

  # Both approaches are equivalent.

  min_dbh <- list('Pinus halepensis' = 7.5, 'Quercus ilex' = 7.5)
  max_dbh <- list('Pinus halepensis' = 200, 'Quercus ilex' = 170)
  crs = "EPSG:4326"
  a <- start_stands()
  a <- set_parameters(a, country = "spain",
                      integvars = x,
                      min_dbh = min_dbh,
                      max_dbh = max_dbh,
                      crs = crs)
  b <- start_stands(control = list(country = "spain",
                                   integvars = x,
                                   min_dbh = min_dbh,
                                   max_dbh = max_dbh,
                                   crs = crs))
  expect_identical(a, b)

})
