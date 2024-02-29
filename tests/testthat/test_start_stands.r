test_that("We create an sf object for a single stand", {

  # Create silly single stand.
  a <- start_stands()

  # Check classes.
  expect_identical(class(a), c("sf", "data.frame"))

  # Check colnames.
  expect_true(all(c("idplot","date", "stand_type", "seedlings", "saplings", "trees") %in% colnames(a)))

  # Create silly single stand with right or wrong settings.
  expect_error(a <- start_stands(param = list(country = "italy")))


})
