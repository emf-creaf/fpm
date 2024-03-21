test_that("Get attributes of sf tree stand object", {

  # Create sf stand.
  a <- start_stands()

  # Species integvars and h.
  x <- list('Pinus halepensis' = seq(7.5, 200, length = 1000),
            'Quercus ilex' = seq(7.5, 170, length = 1500))
  mindbh <- list('Pinus halepensis' = 7.5, 'Quercus ilex' = 7.5)
  maxdbh <- list('Pinus halepensis' = 120, 'Quercus ilex' = 150)
  crs <-  "EPSG:4326"
  a <- set_parameters(a, param = list(integvars = x,
                                        mindbh = mindbh,
                                        maxdbh = maxdbh,
                                        crs = crs))


  # Tests parameter names one by one.
  expect_identical(get_parameters(a, "country")[[1]], "spain")
  expect_identical(get_parameters(a, "integvars")[[1]], x)
  expect_identical(get_parameters(a, "h")[[1]], sapply(x, function(y) y[2]-y[1], simplify = F))
  expect_identical(get_parameters(a, "mindbh")[[1]], mindbh)
  expect_identical(get_parameters(a, "maxdbh")[[1]], maxdbh)

  # All parameters at the same time.
  p <- get_parameters(a, c("country", "integvars", "h", "mindbh", "maxdbh", "crs"))
  expect_identical(p$country, "spain")
  expect_identical(p$integvars, x)
  expect_identical(p$mindbh, mindbh)
  expect_identical(p$maxdbh, maxdbh)


})
