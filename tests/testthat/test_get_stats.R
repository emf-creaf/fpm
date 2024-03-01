test_that("Make 'sf' with statistics", {

  # source("./R/start_stands.r")
  # source("./R/set_parameters.r")
  # source("./R/build_stands.r")
  # source("./R/smooth_stands.r")
  # source("./R/helpers.r")
  # source("./R/factor_diam_IFN.r")
  # source("./R/get_parameters.r")
  # source("./R/kernsmooth.r")
  # source("./R/get_stats.r")
  # source("./R/calc_ba.r")
  # source("./R/calc_ntrees.r")
  # source("./R/get_species.r")
  # source("./R/dtrexp.r")
  # source("./R/clear_stands.r")
  # source("./R/numquad_vm.r")
  #
  # source("./R/fpm_seedling.r")
  # source("./R/fpm_sapling.r")
  # source("./R/fpm_ingrowth.r")
  # source("./R/fpm_survival.r")
  # source("./R/fpm_growth.r")
  # source("./R/fpm_quadrature.r")
  # source("./R/fpm.r")
  #
  # source("./R/collect_parts.R")

  # Load simulated IFN data.
  load("..\\..\\data\\IFNtrees.Rdata")
  load("..\\..\\data\\IFNseedlings.Rdata")
  load("..\\..\\data\\IFNsaplings.Rdata")

  # load(".\\data\\IFNtrees.Rdata")
  # load(".\\data\\IFNseedlings.Rdata")
  # load(".\\data\\IFNsaplings.Rdata")

  # Initialize stands.
  idplot <- unique(trees$idplot)
  i <- match(idplot, trees$idplot)
  n <- length(idplot)
  a <- start_stands()


  # Now we add tree information for those known (although empty) 20 plots.
  df <- list()
  for (i in idplot) {
    df[[i]] <- trees[trees$idplot == i, c("dbh", "species")]
    a <- build_stands(a, i, data = list(df = df[[i]],
                     data_type = "trees",
                     stand_type = "individual",
                     date = as.Date("2000-01-01")),
                     verbose = F)
  }
  b <- get_stats(a, verbose = F)

  # Convolve to obtain a continuous distribution and update.
  x <- list('Pinus nigra' = seq(7.5,220,length=1000),
            'Pinus halepensis' = seq(7.5,250,length=1500),
            'Quercus ilex' = seq(7.5,250,length=2000))
  a <- set_parameters(a, list(integvars = x))
  aa <- smooth_stands(a, verbose = F)
  bb <- get_stats(aa, verbose = F)

  # Number of trees must be the same (within numerical error).
  expect_lt(all(b$ntrees == bb$ntrees), 0.01)

  # Basal area after convolution must be similar.
  expect_lt(max(abs(1-b$ba/bb$ba)), 0.20)




})
