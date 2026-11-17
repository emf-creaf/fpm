test_that("custom metadata survive dplyr operations", {

  x <- sf::st_as_sf(
    data.frame(
      species = c("A", "B", "A"),
      x = c(1, 2, 3),
      y = c(1, 2, 3)
    ),
    coords = c("x", "y")
  )

  stands_data <- list(
    mindbh = list('Pinus halepensis' = 7.5, 'Quercus ilex' = 7.5),
    maxdbh = list('Pinus halepensis' = 200, 'Quercus ilex' = 170),
    integvars = list('Pinus halepensis' = seq(7.5, 200, length = 1000),
                          'Quercus ilex' = seq(7.5, 170, length = 1500))
  )

  x <- new_stands_sf(
    x,
    stands_data
  )

  expect_true(all(inherits(x, "stands_sf"), inherits(x, "sf")))
  expect_true(all(c("mindbh", "maxdbh", "integvars") %in% names(attr(x, "stands_data"))))
  y <- x |>
    dplyr::filter(species == "A") |>
    dplyr::mutate(foo = 1) |>
    dplyr::select(species, foo, geometry)

  expect_s3_class(y, "stands_sf")
  expect_s3_class(y, "sf")

  expect_identical(
    attr(y, "stands_data"),
    attr(x, "stands_data")
  )



})
