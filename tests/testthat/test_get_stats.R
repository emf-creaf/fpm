test_that("Make 'sf' with statistics", {

  # Load simulated IFN data.
  load("..\\..\\data\\IFNtrees.Rdata")
  load("..\\..\\data\\IFNseedlings.Rdata")
  load("..\\..\\data\\IFNsaplings.Rdata")

  # Initialize stands.
  idplot <- unique(trees$idplot)
  i <- match(idplot, trees$idplot)
  n <- length(idplot)
  a <- start_stands(idplot = idplot, x = trees$utm_x[i], y = trees$utm_y[i], "EPSG:32630")
  a <- set_parameters(a, country = "spain")


  # Now we add tree information for those known (although empty) 20 plots.
  df <- list()
  for (i in idplot) {
    df[[i]] <- trees[trees$idplot == i, c("dbh", "species")]
    a <- build_stand(a, i, df[[i]],
                     data_type = "trees",
                     stand_type = "individual",
                     date = as.Date("2000-01-01"),
                     country = "spain")
  }
  b <- get_stats(a, verbose = F)

  # Convolve to obtain a continuous distribution and update.
  x <- list('Pinus nigra' = seq(7.5,220,length=1000),
            'Pinus halepensis' = seq(7.5,250,length=1500),
            'Quercus ilex' = seq(7.5,250,length=2000))
  a <- set_parameters(a, integvars = x)
  aa <- smooth_stands(a, verbose = F)
  bb <- get_stats(aa, verbose = F)

  # Number of trees must be the same (within numerical error).
  expect_lt(all(b$ntrees == bb$ntrees), 0.01)

  # Basal area after convolution must be similar.
  expect_lt(max(abs(1-b$ba/bb$ba)), 0.20)




})
