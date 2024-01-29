test_that("Checking plots", {


  # First initialize one single stand for the Spanish IFN.
  a <- start_stands(paste0("ID", 1:100), x = runif(100), y = runif(100), "EPSG:4326")
  a <- set_parameters(a, country = "spain")


  # Now we add tree information.
  df <- list()
  for (i in 1:50) {
    dbh <- 7.5+runif(5)*20
    df[[i]] <- data.frame(species = c(sample(c("Pnigra","Phalep"),5,replace=T)),
                          dbh = dbh, factor_diam = factor_diam_IFN(dbh, "area"))
    a <- build_stand(a, paste0("ID",i), df[[i]],
                      data_type = "trees",
                      stand_type = "individual",
                      date = as.Date("2000-01-01"),
                      country = "spain")
  }

  # Add saplings information.
  a <- build_stand(a, "ID3",
                   data.frame(species = c("Pnigra", "Phalep"), n = c(30, 51)),
                   data_type = "saplings",
                   date = as.Date("2000-01-01"),
                   country = "spain")


  # Everything is ok.
  expect_true(all(sapply(1:length(a$idplot), function(i) check_stand(a[i, ]))))


  # Wrong stand_type value.
  b <- a
  b$stand_type <- 98
  expect_warning(check_stand(b[1, ]))

  # Wrong element in column of lists.
  b <- a
  b[25, ]$saplings[[1]] <- 7
  expect_warning(check_stand(b[25, ]))

  # Wrong column names.
  b <- a
  colnames(b[10, ]$trees[[1]]) <- c("rt", "dbh", "factor_diam")
  expect_warning(check_stand(b[10, ]))


})
