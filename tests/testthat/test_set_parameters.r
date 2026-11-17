test_that("Set attributes of sf tree stand object", {

  # Create sf stand.
  a <- sf::st_sf(sf::st_sfc())

  # country is not set here..
  expect_error(set_parameters(a, param = list(crs = "ASDF"), verbose = FALSE))

  # Species integvars and h.
  x <- list('Pinus halepensis' = seq(7.5, 200, length = 1000),
            'Quercus ilex' = seq(7.5, 170, length = 1500))
  a <- set_parameters(a, param = list(integvars = x), verbose = FALSE)

  # # Nothing is done.
  expect_error(set_parameters(a, verbose = FALSE))

  # Wrong integvars.
  expect_error(set_parameters(a, integvars = 1:3, verbose = FALSE))

  # Correct integvars.
  expect_no_error(set_parameters(a, param = list(integvars = list(x=1:3, y=(4:6)/2)), verbose = FALSE))

  # Both approaches are equivalent.
  mindbh <- list('Pinus halepensis' = 7.5, 'Quercus ilex' = 7.5)
  maxdbh <- list('Pinus halepensis' = 200, 'Quercus ilex' = 170)
  crs = "EPSG:4326"
  a <- sf::st_sf(sf::st_sfc())
  a <- set_parameters(a, param = list(integvars = x,
                      mindbh = mindbh,
                      maxdbh = maxdbh,
                      crs = crs))

  expect_all_true(c("mindbh", "maxdbh", "integvars", "h") %in% names(attributes(a)))
  expect_all_true(c('Pinus halepensis', 'Quercus ilex') %in% names(attr(a, "mindbh")))
  expect_all_true(c('Pinus halepensis', 'Quercus ilex') %in% names(attr(a, "maxdbh")))
  expect_all_true(c('Pinus halepensis', 'Quercus ilex') %in% names(attr(a, "integvars")))
  expect_all_true(c('Pinus halepensis', 'Quercus ilex') %in% names(attr(a, "h")))
  expect_true(is.numeric(attr(a, "integvars")[['Pinus halepensis']]))

})
