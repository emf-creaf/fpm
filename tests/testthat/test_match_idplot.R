test_that("Match idplot identifier", {

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

  b <- update_all(a, verbose = F)

  # data.frame with predictors.
  df <- data.frame(idplot = a$idplot)

  # Tests.
  expect_identical(a$idplot, match_idplot(a, df, verbose = F))
  expect_true(is.na(match_idplot(a, data.frame(idplot = c("hh34", "ASDF")), verbose = F)))

})
