test_that("Smoothing discrete tree data", {

  # First initialize one single stand for the Spanish IFN.
  a <- start_stands(paste0("ID", 1:100), x = runif(100), y = runif(100), "EPSG:4326")
  a <- set_parameters(a, country = "spain")


  # Now we add tree information.
  df <- list()
  for (i in 1:100) {
    dbh <- 7.5+runif(5)*20
    df[[i]] <- data.frame(species = c(sample(c("Pnigra","Phalep"),5,replace=T)),
                          dbh = dbh, factor_diam = factor_diam_IFN(dbh, "area"))
    a <- build_stand(a, paste0("ID",i), df[[i]],
                      data_type = "trees",
                      stand_type = "individual",
                      date = as.Date("2000-01-01"),
                      country = "spain")
  }


  # Convolve to obtain a continuous distribution and update.
  x <- list(Pnigra = seq(7.5,200,length=1000), Phalep = seq(7.5,250,length=1500))
  a <- set_parameters(a, integvars = x)
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

  # Check that smooth_stand has not modified the number of trees.


})
