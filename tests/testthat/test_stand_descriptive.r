test_that("Descriptive statistics", {

   # First initialize one single stand.
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

   # Convolve to obtain a continuous distribution.
   x <- data.frame(Pnigra = seq(7.5,200,length=1000), Phalep = seq(7.5,250,length=1000))
   a <- set_attributes(a, integvars = x)
   b <- smooth_stand(a)

   # Update descriptive statistics for continuous data.
   a <- stand_descriptive(a)
   b <- stand_descriptive(b)

   # Check initial species are still there.
   expect_true(all(sapply(1:nrow(b), function(i) all(colnames(b[i,]$trees[[1]]) %in% a[i,]$trees[[1]]$species))))

   # Check species are the same.
   ab <- sapply(1:nrow(a), function(i) {
     colna <- colnames(a$N_species[[i]])
     colnb <- colnames(b$N_species[[i]])
     j1 <- match(colna, colnb)
     j2 <- match(colnb, colna)
     all(!is.na(j1), !is.na(j2))
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
