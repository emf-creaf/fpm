test_that("Get attributes of sf tree stand object", {

  # Create sf stand.
  a <- start_stands()

  # Species integvars and h.
  x <- list('Pinus halepensis' = seq(7.5, 200, length = 1000),
            'Quercus ilex' = seq(7.5, 170, length = 1500))
  min_dbh <- list('Pinus halepensis' = 7.5, 'Quercus ilex' = 7.5)
  max_dbh <- list('Pinus halepensis' = 120, 'Quercus ilex' = 150)
  crs <-  "EPSG:4326"
  a <- set_parameters(a, control = list(integvars = x,
                                        min_dbh = min_dbh,
                                        max_dbh = max_dbh,
                                        crs = crs))


  # Tests parameter names one by one.
  expect_identical(get_parameters(a, "country"), "spain")
  expect_identical(get_parameters(a, "integvars"), x)
  expect_identical(get_parameters(a, "h"), sapply(x, function(y) y[2]-y[1], simplify = F))
  expect_identical(get_parameters(a, "min_dbh"), min_dbh)
  expect_identical(get_parameters(a, "max_dbh"), max_dbh)

  # All parameters at the same time.
  p <- get_parameters(a, c("country", "integvars", "h", "min_dbh", "max_dbh", "crs"))
  expect_identical(p$country, "spain")
  expect_identical(p$integvars, x)
  expect_identical(p$min_dbh, min_dbh)
  expect_identical(p$max_dbh, max_dbh)


})
