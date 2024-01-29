test_that("Building tree stands", {


  # First initialize one single stand for the Spanish IFN.
  a <- start_stands(paste0("ID", 1:10), x = runif(10), y = runif(10), "EPSG:4326")
  a <- set_parameters(a, country = "spain")


  # Now we add tree information.
  df <- list()
  for (i in 1:2) {
    df[[i]] <- data.frame(species = c(sample(c("Pnigra","Phalep"),5,replace=T)), dbh = 7.5+runif(5)*20)
    a <- build_stand(a, paste0("ID",i), df[[i]],
                     data_type = "trees",
                     stand_type = "individual",
                     date = as.Date("2000-01-01"),
                     country = "spain")
  }


  # Tree data have been successfully saved in 'a'.
  for (i in 1:2) {
    x <- df[[i]]
    expect_identical(a$trees[[i]], x)
  }


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




})
