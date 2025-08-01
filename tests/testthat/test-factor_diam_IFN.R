test_that("Correction factor for varying stand radius", {

  expect_error(factor_diam_IFN(letters[1:2]))
  expect_error(factor_diam_IFN(c(-3.4, 0, 7, 8.1)))
  x <- c(0.1, 7.1, 8.9, 14.2, 31.9, 154.3)
  expect_equal(factor_diam_IFN(x), c(NA, NA, 127.323954, 31.830989, 14.147106, 5.092958))
  expect_identical(factor_diam_IFN(x, radius_correction = FALSE), rep(1, length(x)))

})
