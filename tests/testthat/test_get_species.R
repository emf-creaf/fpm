test_that("Building tree stands", {

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


  # Get species and check them out.
  b <- get_species(a, verbose = F)
  expect_true(all(attr(b, "species") %in% c("Pinus nigra", "Pinus halepensis", "Quercus ilex")))
  expect_true(all(c("Pinus nigra", "Pinus halepensis", "Quercus ilex") %in% attr(b, "species")))

})
