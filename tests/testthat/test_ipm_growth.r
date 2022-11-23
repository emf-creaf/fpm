test_that("IPM growth terms", {

  # First initialize one single stand.
  a <- start_stand("ID1", 5, 45, "EPSG:4326")
  a <- set_attributes(a)

  # Next, we merge other stands.
  for (i in paste0("ID",2:10)) {
    b <- start_stand(i, 5, 45, "EPSG:4326")
    b <- set_attributes(b)
    a <- merge_stands(a,b)
  }

  # Now we add tree information.
  for (i in paste0("ID",1:10)) {
    df <- data.frame(species = c(sample(c("Pnigra","Phalep"),5,replace=T)),
                     dbh1 = 7.5+runif(5)*20, factor_diam1 = sample(c(127.324, 31.83099),5,replace=T))
    a <- build_stand(a, i, df, "trees", "individual", 1990)
  }

  # Convolve to obtain a continuous distribution.
  x <- data.frame(Pnigra = seq(7.5,200,length=1000),
                  Phalep = seq(7.5,250,length=1000),
                  Qilex = seq(7.5,250,length=1000))
  a <- set_attributes(a, integvars = x)
  ainit <- stand_descriptive(a)
  a <- smooth_stand(a)

  # IPM functions.
  gr <- data.frame(Pnigra=rep(.1, nrow(x)), Phalep=rep(.15, nrow(x)))
  va <- data.frame(Pnigra=rep(2, nrow(x)), Phalep=rep(2.5, nrow(x)))
  su <- data.frame(Pnigra=rep(1, nrow(x)), Phalep=rep(1, nrow(x)))

  # Apply quadrature.
  min_dbh <- c(Pnigra = 7.5, Phalep = 7.5, Qilex = 7.5)
  b <- ipm_quadrature(a, "ID1", gr, va, su, min_dbh = min_dbh)
  for (i in paste0("ID",2:10)) b <- ipm_quadrature(b, i, gr, va, su, min_dbh = min_dbh)

  # Ingrowth calculation.
  num_sapl <- c(Pnigra = 4, Phalep = 8, Qilex = 10)
  lambda <- c(Pnigra = .4, Phalep = .7, Qilex = .3)

  b2 <- ipm_ingrowth(b, "ID8", num_sapl, lambda, min_dbh, scaled_ha = F)

  # Test Qilex.
  q <- MiscStat::dtrexp(x[, "Qilex"], lambda["Qilex"], min = min_dbh["Qilex"]) * num_sapl["Qilex"]
  expect_equal(sum(abs(q-b2$trees[[8]][,3])), 0)

})
