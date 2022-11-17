test_that("Smoothing discrete tree data", {

  # First initialize one single stand.
  a <- start_stand("ID1", 5, 45, "EPSG:4326")
  a <- set_attributes(a)

  # Next, we merge other stands.
  for (i in 2:10) {
    b <- start_stand(paste0("ID",i), 5, 45, "EPSG:4326")
    b <- set_attributes(b)
    a <- merge_stands(a,b)
  }

  # Now we add tree information.
  for (i in 1:10) {
    df <- data.frame(species = c(sample(c("Pnigra","Phalep"),5,replace=T)),
    dbh1 = 7.5+runif(5)*20, factor_diam1 = sample(c(127.324, 31.83099),5,replace=T))
    a <- build_stand(a, paste0("ID",i), df, "trees", "individual", 1990)
  }

  # Convolve to obtain a continuous distribution.
  x <- data.frame(Pnigra = seq(7.5,200,length=1000), Phalep = seq(7.5,250,length=1000))
  a <- set_attributes(a, integvars = x)
  b <- smooth_stand(a)

  # Check classes.
  expect_identical(class(b), c("sf", "data.frame"))

  # Check data.frame class.
  expect_identical(sapply(1:nrow(b), function(i) class(a[i,]$trees[[1]])),
                   rep("data.frame",nrow(b)))

  # Check type of stand.
  expect_true(all(sapply(1:nrow(b), function(i) b[i,]$stand_type == "ipm")))

  # Check number of rows in tree data.frames.
  expect_true(all(sapply(1:nrow(b), function(i) nrow(b[i,]$trees[[1]])) == nrow(x)))

  # Check number of columns in tree data.frames.
  expect_true(all(sapply(1:nrow(b), function(i) ncol(b[i,]$trees[[1]]) == length(unique(a[i,]$trees[[1]]$species)))))

  # Check initial species are still there.
  expect_true(all(sapply(1:nrow(b), function(i) colnames(b[i,]$trees[[1]]) %in% a[i,]$trees[[1]]$species)))

  # Check that smooth_stand has not generated any NA's.
  expect_true(all(sapply(1:nrow(b), function(i) all(!is.na(b[i,]$trees[[1]])))))

  # Check that smooth_stand has not modified the number of trees.


})
