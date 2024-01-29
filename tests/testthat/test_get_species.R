test_that("Building tree stands", {

  # First initialize one single stand for the Spanish IFN.
  a <- start_stands(paste0("ID", 1:10), x = runif(10), y = runif(10), "EPSG:4326")
  a <- set_parameters(a, country = "spain")


  # Build stands.
  df <- list()
  for (i in 1:2) {
    df[[i]] <- data.frame(species = c(sample(c("Pnigra","Phalep"),5,replace=T)),
                          dbh = 7.5+runif(5)*20, factor_diam = sample(c(127.324, 31.83099),5,replace=T))

    # Add tree information.
    a <- build_stand(a, paste0("ID",i), df[[i]],
                      data_type = "trees",
                      stand_type = "individual",
                      date = as.Date("2000-01-01"),
                      country = "spain")

    # Add saplings information.
    a <- build_stand(a, "ID1",
                     data.frame(species = c("Pnigra", "Phalep"),  n = c(30, 51)),
                     data_type = "saplings",
                     stand_type = "individual",
                     date = as.Date("2000-01-01"),
                     country = "spain")
  }


  b <- get_species(a)

})
