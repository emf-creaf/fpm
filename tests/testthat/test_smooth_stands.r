test_that("Smoothing discrete tree data", {

  # Load simulated IFN data.
  load("..\\..\\data\\IFNtrees.Rdata")
  load("..\\..\\data\\IFNseedlings.Rdata")
  load("..\\..\\data\\IFNsaplings.Rdata")

  # load(".\\data\\IFNtrees.Rdata")
  # load(".\\data\\IFNseedlings.Rdata")
  # load(".\\data\\IFNsaplings.Rdata")


  # Initialize only 20 stands.
  idplot <- unique(trees$idplot)
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
  x <- list('Pinus nigra' = seq(7.5,220,length=1000),
            'Pinus halepensis' = seq(7.5,250,length=1500),
            'Quercus ilex' = seq(7.5,250,length=2000))
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
  # sa <- get_stats(a, verbose = F)
  # sb <- get_stats(b, verbose = F)
  #
  #
  #
  # # Check that seedlings and saplings have not been modified.
  # expect_true(all(sapply(idplot, function(x) identical(a[[x]]$seedlings, b[[x]]$seedlings))))
  # expect_true(all(sapply(idplot, function(x) identical(a[[x]]$saplings, b[[x]]$saplings))))

})
