test_that("Get attributes of sf tree stand object", {

  # Create sf stand.
  a <- start_stands()

  # Set parameters.
  a <- set_parameters(a, country = "spain", crs =  "EPSG:4326")
  x <- list('Pinus halepensis' = seq(7.5, 200, length = 1000),
            'Quercus ilex' = seq(7.5, 170, length = 1500))
  min_dbh <- list('Pinus halepensis' = 7.5, 'Quercus ilex' = 7.5)
  max_dbh <- list('Pinus halepensis' = 120, 'Quercus ilex' = 150)
  a <- set_parameters(a, country = "spain",
                      integvars = x,
                      min_dbh = min_dbh,
                      max_dbh = max_dbh)

  # Tests parameter names one by one.
  expect_identical(get_parameters(a, "country"), "spain")
  expect_identical(get_parameters(a, "integvars"), x)
  expect_identical(get_parameters(a, "h"), sapply(x, function(y) y[2]-y[1], simplify = F))
  expect_identical(get_parameters(a, "min_dbh"), min_dbh)
  expect_identical(get_parameters(a, "max_dbh"), max_dbh)
  expect_identical(colnames(sf::st_coordinates(a)), c("X","Y"))
  expect_identical(sf::st_crs(a)$input, "EPSG:4326")
#   b <- start_stands("ID10", 5, 45, "EPSG:4326")
#   expect_equal(sf::st_coordinates(a), "EPSG:4326")
#
#   # Check bounding box.
#   bb <- sf::st_bbox(a)
#   expect_identical(class(bb), "bbox")
#   expect_equal(as.vector(bb), c(5,44,6,45))

})
