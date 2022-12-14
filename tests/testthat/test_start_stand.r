test_that("We create an sf object for a single stand", {

  # Create silly single stand.
  a <- start_stand("ID1", 5, 45, "EPSG:4326")

  # Check classes.
  expect_identical(class(a), c("sf", "data.frame"))

  # Check reference system.
  expect_identical(sf::st_crs(a), sf::st_crs("EPSG:4326"))

  # Check colnames.
  expect_setequal(colnames(a), c("idplot", "geometry", "idplot", "stand_type", "date", "species",
                                  "trees","seedlings",
                                  "saplings","N_species","BA_species",
                                  "N_stand","BA_stand"))

  # Check coordinates.
  expect_identical(colnames(sf::st_coordinates(a)), c("X","Y"))
  b <- matrix(c(5,45),1,2)
  colnames(b) <- c("X","Y")
  rownames(b) <- "1"
  expect_equal(sf::st_coordinates(a), b)

  # Check bounding box.
  bb <- sf::st_bbox(a)
  expect_identical(class(bb), "bbox")
  expect_equal(as.vector(bb), c(5,45,5,45))

})
