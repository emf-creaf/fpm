test_that("Descriptive statistics", {

  library(dplyr)

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

   # Update descriptive statistics for continuous data.
   a <- stand_descriptive(a)
   b <- stand_descriptive(b)


})
