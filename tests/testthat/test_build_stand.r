test_that("Building tree stands", {

  # Load simulated IFN data.
  load("..\\..\\data\\IFNtrees.Rdata")
  load("..\\..\\data\\IFNseedlings.Rdata")
  load("..\\..\\data\\IFNsaplings.Rdata")

  # Initialize only 20 stands.
  idplot <- unique(trees$idplot)
  i <- match(idplot, trees$idplot)
  n <- length(idplot)
  a <- start_stands(idplot = idplot[1:20], x = trees$utm_x[i[1:20]], y = trees$utm_y[i[1:20]], "EPSG:32630")
  a <- set_parameters(a, country = "spain")


  # Now we add tree information for those known (although empty) 20 plots.
  df <- list()
  for (i in idplot[1:20]) {
    df[[i]] <- trees[trees$idplot == i, c("dbh", "species")]
    a <- build_stand(a, i, df[[i]],
                     data_type = "trees",
                     stand_type = "individual",
                     date = as.Date("2000-01-01"),
                     country = "spain")
  }

  # Next, tree information for 20 new plots.
  for (i in idplot[21:40]) {
    df[[i]] <- trees[trees$idplot == i, c("dbh", "species")]
    a <- build_stand(a, i, df[[i]],
                     data_type = "trees",
                     stand_type = "individual",
                     date = as.Date("2000-01-01"),
                     country = "spain")
  }

  # Seedlings in old and new plots.
  seedlings$n <- seedlings$n/3
  for (i in idplot[1:60]) {
    z <- seedlings[seedlings$idplot == i, c("species", "n")]
    if (nrow(z) > 0) {
      a <- build_stand(a, i, z,
                       data_type = "seedlings",
                       stand_type = "individual",
                       date = as.Date("2000-01-01"),
                       country = "spain")
    }
  }


  # Saplings in old and new plots.
  for (i in idplot[1:80]) {
    z <- saplings[saplings$idplot == i, c("species", "n")]
    if (nrow(z) > 0) {
      a <- build_stand(a, i, z,
                       data_type = "saplings",
                       stand_type = "individual",
                       date = as.Date("2000-01-01"),
                       country = "spain")
    }
  }


  # Saplings in plots with trees but without seedlings.
  for (i in idplot[81:100]) {
    df[[i]] <- trees[trees$idplot == i, c("dbh", "species")]
    a <- build_stand(a, i, df[[i]],
                     data_type = "trees",
                     stand_type = "individual",
                     date = as.Date("2000-01-01"),
                     country = "spain")
  }
  for (i in idplot[81:100]) {
    z <- saplings[saplings$idplot == i, c("species", "n")]
    if (nrow(z) > 0) {
      a <- build_stand(a, i, z,
                       data_type = "saplings",
                       stand_type = "individual",
                       date = as.Date("2000-01-01"),
                       country = "spain")
    }
  }


  # Tree data have been successfully saved in 'a'.
  for (j in idplot[c(1:40, 81:100)]) expect_identical(a[a$idplot==j,]$trees[[1]], df[[j]][c("species", "dbh")])


  # Check classes.
  expect_identical(class(a), c("sf", "data.frame"))


  # Check data.frame class.
  for (j in i) expect_identical(class(a[a$idplot==j,]$trees[[1]]), "data.frame")


  # Check stand_type.
  for (i in i) expect_true(any(a[a$idplot==j,]$stand_type == "individual"))


  # We add a new stand and check that it is ok.
  a <- build_stand(a, "id200",
                   a[1, ]$trees[[1]],
                   data_type = "trees",
                   stand_type = "individual",
                   date = as.Date("2000-01-01"),
                   country = "spain")


  # Fails when seedlings or saplings are negative.
  expect_error(a <- build_stand(a, "id3",
                                data.frame(species = c("Pinus nigra", "Pinus halepensis"), n = -c(1, 1)),
                                data_type = "saplings",
                                stand_type = "individual",
                                date = as.Date("2000-01-01"),
                                country = "spain"))


  expect_error(a <- build_stand(a, "ID2",
                                data.frame(species = c("Pinus nigra", "Pinus halepensis"), n = - runif(2)),
                                data_type = "seedlings",
                                stand_type = "individual",
                                date = as.Date("2000-01-01"),
                                country = "spain"))


  # Fail when Date is missing.
  expect_error(a <- build_stand(a, "ID2",
                                data.frame(species = c("Pinus nigra", "Pinus halepensis"), n = c(1, 1)),
                                data_type = "saplings",
                                stand_type = "individual",
                                country = "spain"))


})
