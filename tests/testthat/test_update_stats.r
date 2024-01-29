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
  a <- update_species(a, verbose = F)

  # Some silly test.
  # system.time(replicate(100, {
  #   for (i in 1:100) {
  #     b1 <- a[i, ]$plot[[1]]
  #   }
  # }))
  # system.time(replicate(100, {
  #   for (i in 1:100) {
  #     b2 <- a$plot[[i]]
  #   }
  # }))


  # Update descriptive statistics and species list.
  a <- a |> update_stats(verbose = F)


  # Convolve to obtain a continuous distribution.
  x <- list(Pnigra = seq(7.5,200,length=1000), Phalep = seq(7.5,250,length=1500))
  a <- set_parameters(a, integvars = x)
  b <- smooth_stands(a, verbose = F)




  # Check initial species are still there.
  expect_true(all(sapply(1:nrow(b), function(i) all(colnames(b[i,]$trees$trees[[1]]) %in% a[i,]$species$trees[[1]]))))


  # Check species are the same.
  a <- update_species(a, verbose = F)
  b <- update_species(b, verbose = F)
  ab <- sapply(1:nrow(a), function(i) {
    colna <- a$plot[[i]]$species$trees
    colnb <- b$plot[[i]]$species$trees
    j1 <- match(colna, colnb)
    j2 <- match(colnb, colna)
    all(!is.na(j1), !is.na(j2), length(j1)>0, length(j2)>0)
  })
  expect_true(all(ab))

  # Check descriptive statistics. Difference must be less than 0.1%.
  # Change this criteria if necessary.
  ab <- sapply(1:nrow(a), function(i) {
    j <- match(a$N_species[[i]]$species, b$N_species[[i]]$species)
    sum(abs((a$N_species[[i]]$N - b$N_species[[i]]$N[j])/a$N_species[[i]]$N))*100
  })
  expect_lt(max(ab), 0.1)

})
