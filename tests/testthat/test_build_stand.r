test_that("Building tree stands", {

  # First initialize one single stand for the Spanish IFN.
  a <- start_stand("ID1", 5, 45, "EPSG:4326")
  a <- set_attributes(a, country = "spain")

  # Next, we merge other stands.
  for (i in 2:10) {
    b <- start_stand(paste0("ID",i), 5, 45, "EPSG:4326")
    b <- set_attributes(b, country = "spain")
    a <- merge_stands(a,b)
  }

  # Now we add tree information.
  for (i in 1:10) {
    df <- data.frame(species = c(sample(c("Pnigra","Phalep"),5,replace=T)),
    dbh1 = 7.5+runif(5)*20, factor_diam1 = sample(c(127.324, 31.83099),5,replace=T))
    a <- build_stand(a, paste0("ID",i), df,
                     data_type = "trees",
                     stand_type = "individual",
                     date = 2000,
                     country = "spain")
  }

  # Check classes.
  expect_identical(class(a), c("sf", "data.frame"))

  # Check data.frame class.
  expect_identical(sapply(1:nrow(a), function(i) class(a[i,]$trees[[1]])),
                   rep("data.frame",nrow(a)))

  # Check number of trees.
  expect_equal(sapply(1:nrow(a), function(i) nrow(a[i,]$trees[[1]])),
                   rep(5,nrow(a)))

  # Check date.
  expect_equal(sapply(1:nrow(a), function(i) a[i,]$date), rep(2000,nrow(a)))

  # Check stand_type.
  expect_equal(sapply(1:nrow(a), function(i)a[i,]$stand_type), rep("individual",nrow(a)))

})
