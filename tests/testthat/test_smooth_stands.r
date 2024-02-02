test_that("Smoothing discrete tree data", {

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




  # Convolve to obtain a continuous distribution and pdate.
  x <- list('Pinus nigra' = seq(7.5,220,length=1000),
            'Pinus halepensis' = seq(7.5,250,length=1500),
            'Quercus ilex' = seq(7.5,250,length=2000))
  a <- set_parameters(a, integvars = x)
  b <- smooth_stands(a, verbose = F)




  # Check classes.
  expect_identical(class(b), c("sf", "data.frame"))

  # Check type of stand.
  expect_true(all(sapply(1:nrow(b), function(i) b$stand_type == "ipm")))

  # Check number of rows in tree data.frames.
  expect_true(all(sapply(1:nrow(b), function(i) nrow(b$trees[[i]])) == nrow(x)))

  # Check number of columns in tree list.
  expect_true(all(sapply(1:nrow(b), function(i) length(b$trees[[i]]) == length(unique(a$trees[[i]]$species)))))

  # Check initial species are still there.
  expect_true(all(sapply(1:nrow(b), function(i) all(names(b$trees[[i]]) %in% a$trees[[i]]$species))))

  # Check that smooth_stand has not generated any NA's.
  expect_true(all(sapply(1:nrow(b), function(i) all(!is.na(b$trees[[i]]$trees)))))

  # Check that smooth_stand has not modified the number of trees.


})
