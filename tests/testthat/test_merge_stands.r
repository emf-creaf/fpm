test_that("Merging stands", {

  a <- start_stands("ID1", 5, 45, "EPSG:4326")
  a <- set_parameters(a, "spain")

  b <- start_stands(paste0("ID",2), 15, 45, "EPSG:4326")
  b <- set_parameters(b, "spain")
  a <- merge_stands(a, b, update = F) # No update because there are no trees yet.

  # Attribute "country" is present and correct after merge.
  expect_identical(attr(a, "country"), "spain")

  # Two plots are present.
  expect_equal(nrow(a), 2)

})
