test_that("Updating stands", {

  # First initialize one single stand for the Spanish IFN.
  a <- start_stands(paste0("ID", 1:100), x = runif(100), y = runif(100), "EPSG:4326")
  a <- set_parameters(a, country = "spain")


  # Now we add tree information.
  df <- list()
  for (i in 1:100) {
    dbh <- 7.5+runif(5)*20
    df[[i]] <- data.frame(species = c(sample(c("Pnigra","Phalep"),5,replace=T)),
                          dbh = dbh, factor_diam = factor_diam_IFN(dbh, "area"))
    a <- build_stands(a, paste0("ID",i), df[[i]],
                      data_type = "trees",
                      stand_type = "individual",
                      date = as.Date("2000-01-01"),
                      country = "spain")
  }

  # Add saplings information.
  a[3, ]$plot[[1]]$saplings <- data.frame(species = c("Pnigra", "Phalep"),
                                          N = c(30, 51))

  # Update species.
  b <- update_species(a, verbose = F)


  # Test.
  expect_in(b$plot[[10]]$species$trees, c("Pnigra","Phalep"))
  expect_contains(c("Pnigra", "Phalep"), b$plot[[3]]$species$saplings)


  # Continuous version.
  x <- list(Pnigra = seq(7.5,200,length=1000), Phalep = seq(7.5,250,length=1500))
  a <- set_parameters(a, integvars = x)
  b <- a |> smooth_stands(verbose = F) |> update_species(verbose = F)


  # Test.
  expect_in(b$plot[[10]]$species$trees, c("Pnigra","Phalep"))
  expect_in(b$plot[[3]]$species$saplings, c("Pnigra", "Phalep"))

})
