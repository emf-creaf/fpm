test_that("Smoothing discrete tree data", {

  # Load simulated IFN data.
  load("..\\..\\data\\trees.Rdata")
  load("..\\..\\data\\seedlings.Rdata")
  load("..\\..\\data\\saplings.Rdata")

  # load(".\\data\\trees.Rdata")
  # load(".\\data\\seedlings.Rdata")
  # load(".\\data\\saplings.Rdata")

  # Seedlings. First we average duplicated rows.
  seedlings$n <- seedlings$n/3
  seedlings <- seedlings |>
    dplyr::group_by(idplot, species) |>
    dplyr::summarise(n = mean(n), .groups = "drop")

  # Saplings. We also average duplicated rows.
  saplings$n <- saplings$n/3
  saplings <- saplings |>
    dplyr::group_by(idplot, species) |>
    dplyr::summarise(n = mean(n), .groups = "drop")

  # Initialize.
  idplot <- unique(trees$idplot)[1]
  i <- match(idplot, trees$idplot)
  n <- length(idplot)
  a <- start_stands()
  a <- set_parameters(a, param = list(crs = "EPSG:32630"))

  # Now we add tree information for those plots.
  df <- list()
  for (i in idplot) {
    df[[i]] <- trees[trees$idplot == i, c("dbh", "species")]
    a <- build_stands(a, i, data = list(df = df[[i]],
                     data_type = "trees",
                     stand_type = "individual",
                     date = as.Date("2000-01-01")), verbose = F)
  }

  # Seedlings.
  seedlings$n <- seedlings$n/3
  for (i in idplot) {
    z <- seedlings[seedlings$idplot == i, c("species", "n")]
    if (nrow(z) > 0) {
      a <- build_stands(a, i, data = list(df = z,
                       data_type = "seedlings",
                       stand_type = "individual",
                       date = as.Date("2000-01-01")))
    }
  }

  # Saplings.
  for (i in idplot) {
    z <- saplings[saplings$idplot == i, c("species", "n")]
    if (nrow(z) > 0) {
      a <- build_stands(a, i, data = list(df = z,
                       data_type = "saplings",
                       stand_type = "individual",
                       date = as.Date("2000-01-01")))
    }
  }


  # Convolve to obtain a continuous distribution and pdate.
  species <- unique(trees$species)
  mindbh <- setNames(rep(7.5, length(species)), species)
  maxdbh <- setNames(sample(150:200, length(species)), species)
  x <- integvars(mindbh, maxdbh, by = .1)

  a <- set_parameters(a, param = list(integvars = x))
  b <- smooth_stands(a, verbose = F)

  # Check classes.
  expect_identical(class(b), c("sf", "data.frame"))

  # Check type of stand.
  expect_true(all(sapply(1:nrow(b), function(i) b$stand_type == "ipm")))

  # Check number of rows in tree data.frames.
  expect_true(all(sapply(1:nrow(b), function(i) nrow(b$trees[[i]])) == nrow(x)))

  # Check number of columns in tree list.
  expect_true(all(sapply(1:nrow(b), function(i) length(b$trees[[i]]) == length(unique(a$trees[[i]]$species)))))

  # Check initial species are still there.
  expect_true(all(sapply(1:nrow(b), function(i) all(names(b$trees[[i]]) %in% a$trees[[i]]$species))))

  # Check that smooth_stand has not generated any NA's.
  expect_true(all(sapply(1:nrow(b), function(i) all(!is.na(b$trees[[i]]$trees)))))

  # # Check that smooth_stand has not modified the number of trees.
  sa <- get_stats(a, verbose = F)
  sb <- get_stats(b, verbose = F)
  expect_true(all(abs((sa$ntrees-sb$ntrees)/((sa$ntrees+sb$ntrees)/2)) < 1e-4))

  # # Check that seedlings and saplings have not been modified.
  expect_true(all.equal(a$seedlings, b$seedlings))
  expect_true(all.equal(a$saplings, b$saplings))

  ###########################3
  # The same, but without the varying radius correction.

  # Initialize.
  idplot <- unique(trees$idplot)[1]
  i <- match(idplot, trees$idplot)
  n <- length(idplot)
  a <- start_stands()
  a <- set_parameters(a, param = list(crs = "EPSG:32630"))

  # Now we add tree information for those plots.
  df <- list()
  for (i in idplot) {
    df[[i]] <- trees[trees$idplot == i, c("dbh", "species")]
    a <- build_stands(a, i, data = list(df = df[[i]],
                                        data_type = "trees",
                                        stand_type = "individual",
                                        date = as.Date("2000-01-01")), verbose = F)
  }
  a <- set_parameters(a, param = list(integvars = x))
  b <- smooth_stands(a, verbose = F, radius_correction = FALSE)

  sa <- get_stats(a, verbose = F, radius_correction = FALSE)
  sb <- get_stats(b, verbose = F, radius_correction = FALSE)
  print(data.frame(sa$ntrees, sb$ntrees))

})
