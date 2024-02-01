test_that("Building tree stands", {

  # Load simulated IFN data.
  load("..\\..\\data\\IFNtrees.Rdata")


  # Initialize stands.
  idplot <- unique(trees$idplot)
  i <- match(idplot, trees$idplot)
  n <- length(idplot)
  a <- start_stands(idplot = idplot, x = trees$utm_x[i], y = trees$utm_y[i], "EPSG:32630")
  a <- set_parameters(a, country = "spain")


  # Now we add tree information.
  df <- list()
  for (i in idplot) {
    df[[i]] <- trees[trees$idplot == i, c("dbh", "species")]
    a <- build_stand(a, i, df[[i]],
                     data_type = "trees",
                     stand_type = "individual",
                     date = as.Date("2000-01-01"),
                     country = "spain")
  }


  # Tree data have been successfully saved in 'a'.
  for (j in i) expect_identical(a[a$idplot==j,]$trees[[1]], df[[j]])


  # Check classes.
  expect_identical(class(a), c("sf", "data.frame"))


  # Check data.frame class.
  for (j in i) expect_identical(class(a[a$idplot==j,]$trees[[1]]), "data.frame")


  # Check stand_type.
  for (i in i) expect_true(any(a[a$idplot==j,]$stand_type %in% c("", "individual")))


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
