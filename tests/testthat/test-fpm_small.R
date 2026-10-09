test_that("Test fpm_small", {

  expect_error(fpm_small("adsf"))

  coords <- matrix(c(
    -122.4194, 37.7749,  # Point 1 (SF)
    -118.2437, 34.0522,  # Point 2 (LA)
    -121.4944, 38.5816   # Point 3 (Sacramento)
  ), ncol = 2, byrow = TRUE)

  plots_sf <- sf::st_sf(
    geometry = sf::st_sfc(
      sf::st_point(coords[1, ]),
      sf::st_point(coords[2, ]),
      sf::st_point(coords[3, ])
    ),
    crs = 4326 # WGS 84 coordinate reference system
  )

  expect_error(fpm_small(plots_sf))

  plots_sf <- sf::st_sf(
    idplot = c("PLOT_001", "PLOT_002", "PLOT_003"),
    geometry = sf::st_sfc(
      sf::st_point(coords[1, ]),
      sf::st_point(coords[2, ]),
      sf::st_point(coords[3, ])
    ),
    crs = 4326 # WGS 84 coordinate reference system
  )
  expect_error(fpm_small(plots_sf, "none"))
  expect_error(fpm_small(plots_sf, "seedlings"))
  expect_error(fpm_small(plots_sf, "seedlings", df = list(a=1)))
  expect_error(fpm_small(plots_sf, "seedlings", df = data.frame(x=1)))

})
