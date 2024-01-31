test_that("Building tree stands", {

  # Load simulated IFN data.
  load("..\\..\\data\\IFNtrees.Rdata")


  # Initialize stands.
  idplot <- unique(trees$idplot)
  n <- length(idplot)
  a <- start_stands(idplot = idplot, x = runif(n), y = runif(n), "EPSG:4326")
  a <- set_parameters(a, country = "spain")


  # Plot identifiers.


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
  for (i in idplot) expect_identical(a[a$idplot == i,]$trees[[1]], df[[i]])



  # Cannot save in plot that does not exist in 'a'.
  expect_error(build_stand(a, "ID25", df[[1]],
                           data_type = "trees",
                           stand_type = "individual",
                           date = as.Date("2000-01-01"),
                           country = "spain")
  )


  # Check classes.
  expect_identical(class(a), c("sf", "data.frame"))


  # Check data.frame class.
  for (i in 1:2) expect_identical(class(a$trees[[i]]), "data.frame")


  # Check stand_type.
  for (i in 1:2) expect_equal(a$stand_type[[i]], "individual")

  # Check all stands.
  expect_true(all(sapply(1:nrow(a), function(x) check_stand(a[i, ]))))

  # Fails when seedlings or saplings are negative.
  expect_error(a <- build_stand(a, "ID2",
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
