test_that("Testing basal area extraction", {

  # First initialize one single stand for the Spanish IFN.
  a <- start_stands(paste0("ID", 1:100), x = runif(100), y = runif(100), "EPSG:4326")
  a <- set_parameters(a, country = "spain")


  # Now we add tree information.
  df <- list()
  for (i in 1:100) {
    df[[i]] <- data.frame(species = c(sample(c("Pnigra","Phalep"),5,replace=T)),
                          dbh = 7.5+runif(5)*20, factor_diam = sample(c(127.324, 31.83099),5,replace=T))
    a <- build_stands(a, paste0("ID",i), df[[i]],
                      data_type = "trees",
                      stand_type = "individual",
                      date = as.Date("2000-01-01"),
                      country = "spain")
  }


  # Convolve to obtain a continuous distribution.
  x <- list(Pnigra = seq(7.5,200,length=5000), Phalep = seq(7.5,250,length=4500))
  a <- set_parameters(a, integvars = x)
  a <- update_stands(a, verbose = F)
  b <- smooth_stands(a, verbose = F)


  # Same length test.
  ba_a <- get_basal_area(a)
  ba_b <- get_basal_area(b)

  expect_length(ba_a, nrow(a))
  expect_length(ba_a, nrow(b))

  # Sum of ba for species test.
  ba_a_sp <- get_basal_area(a, per_species = T)
  ba_b_sp <- get_basal_area(b, per_species = T)

  ba1 <- sapply(ba_a_sp, function(x) sum(unlist(x)))
  ba2 <- sapply(ba_b_sp, function(x) sum(unlist(x)))

  expect_identical(ba_a, ba1)
  expect_identical(ba_b, ba2)
})
