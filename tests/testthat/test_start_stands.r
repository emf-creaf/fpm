test_that("We create an sf object for a single stand", {

  # Create silly single stand.
  sf <- start_stands()

  # Check classes.
  expect_identical(class(sf), c("sf", "data.frame"))

  # Check colnames.
  expect_true(all(c("idplot","date", "stand_type", "seedlings", "saplings", "trees") %in% colnames(sf)))

  # Create silly single stand with right or wrong settings.
  expect_error(start_stands(param = list(country = "france")))
  expect_error(start_stands(param = list(country = "italy")))


})
