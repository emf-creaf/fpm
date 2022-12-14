test_that("Set attributes of sf tree stand object", {

  # Create sf stand.
  a <- start_stand("ID1", 5, 45, "EPSG:4326")

  # Wrong country.
  expect_error(set_attributes(a, "italy"))

  # Right country.
  a <- set_attributes(a, country = "spain")
  expect_identical(attr(a, "country"), "spain")

  # Nothing is done
  expect_warning(set_attributes(a))

  # Wrong integvars.
  expect_error(set_attributes(a, country = "spain", integvars = 1:3))

  # Correct integvars.
  expect_no_error(set_attributes(a, country = "spain", integvars = data.frame(x=1:3, y=4:6)))

})
