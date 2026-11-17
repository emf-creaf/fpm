test_that("Checking that we can set parameters of a sf object", {

  # Simple sf object.
  sf_plots <- sf::st_sf(
    id = 1,
    name = "Barcelona",
    geometry = sf::st_sfc(sf::st_point(c(2.1734, 41.3851)))
  )

  mindbh <- list('Pinus nigra' = 7.5, 'Quercus ilex' = 7.5)
  maxdbh <- list('Pinus nigra' = 150, 'Quercus ilex' = 200)
  integvars <- list('Pinus nigra' = seq(7.5, 150, length = 100),
                    'Quercus ilex' = seq(7.5, 200, length = 150))

  # Tests.
  expect_error(set_parameters())
  expect_error(set_parameters(sf_plots))
  expect_error(set_parameters(sf_plots, param = unlist(mindbh)))
  expect_error(set_parameters(sf_plots, param = list(mindbh = mindbh, dummy = maxdbh)))
  expect_no_error(set_parameters(sf_plots, param = list(mindbh = mindbh, maxdbh = maxdbh), verbose = FALSE))




  crs <- "4326"



})
