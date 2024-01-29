test_that("We create an sf object for a single stand", {

  # Create silly single stand.
  a <- start_stands(c("ID1", "ID2"), c(6, 5), c(44, 45), "EPSG:4326")

  # Check classes.
  expect_identical(class(a), c("sf", "data.frame"))

  # Check colnames.
  expect_true(all(c("idplot","date", "stand_type", "seedlings", "saplings", "trees") %in% colnames(a)))

  # Check coordinates.
  expect_identical(colnames(sf::st_coordinates(a)), c("X","Y"))
  b <- start_stands("ID10", 5, 45, "EPSG:4326")
  expect_equal(sf::st_coordinates(a), sf::st_coordinates(a))

  # Check bounding box.
  bb <- sf::st_bbox(a)
  expect_identical(class(bb), "bbox")
  expect_equal(as.vector(bb), c(5,44,6,45))

})
