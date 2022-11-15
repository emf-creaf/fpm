test_that("Numerical quadrature", {

  # Create stand.
  a <- start_stand("ID1", 5, 45, "EPSG:4326")
  a <- set_attributes(a)

  # Randomly-generated trees.
  df <- data.frame(species = c(sample(c("Pnigra","Phalep"),5,replace=T)),
                   dbh1 = 7.5+runif(5)*20, factor_diam1 = sample(c(127.324, 31.83099),5,replace=T))
  a <- build_stand(a, paste0("ID",i), df, "trees", "individual", 1990)


})
