test_that("Merge stands", {

  a <- start_stand("ID1", 5, 45, "EPSG:4326")
  a <- set_attributes(a)

  b <- start_stand(paste0("ID",2), 15, 45, "EPSG:4326")
  b <- set_attributes(b)
  a <- merge_stands(a,b)

  # Attribute "country" is present and correct after merge.
  expect_identical(attr(a, "country"), "spain")

  # Two plots are present.
  expect_equal(nrow(a), 2)

})
