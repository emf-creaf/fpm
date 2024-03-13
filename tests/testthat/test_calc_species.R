test_that("Building tree stands", {
#
#
#   source("./R/start_stands.r")
#   source("./R/set_parameters.r")
#   source("./R/build_stands.r")
#   source("./R/smooth_stands.r")
#   source("./R/helpers.r")
#   source("./R/factor_diam_IFN.r")
#   source("./R/get_parameters.r")
#   source("./R/kernsmooth.r")
#   source("./R/calc_stats.r")
#   source("./R/calc_species.r")
#   source("./R/dtrexp.r")
#   source("./R/clear_stands.r")
#   source("./R/numquad_vm.r")
#   #
#   source("./R/fpm_seedling.r")
#   source("./R/fpm_sapling.r")
#   source("./R/fpm_ingrowth.r")
#   source("./R/fpm_survival.r")
#   source("./R/fpm_growth.r")
#   source("./R/fpm_quadrature.r")
#   source("./R/fpm.r")
#   #
#   source("./R/collect_parts.R")



  # Load simulated IFN data.
  load("..\\..\\data\\trees.Rdata")
  load("..\\..\\data\\seedlings.Rdata")
  load("..\\..\\data\\saplings.Rdata")

  # Load simulated IFN data.
  # load(".\\data\\trees.Rdata")
  # load(".\\data\\seedlings.Rdata")
  # load(".\\data\\saplings.Rdata")

  # Initialize sf object and add 50 stands.
  idplot <- unique(c(trees$idplot, seedlings$idplot, saplings$idplot))
  i <- match(idplot, trees$idplot)
  n <- length(idplot)

  df <- list()
  a <- start_stands()
  for (j in idplot) {
    df[[j]] <- trees[trees$idplot == j, c("dbh", "species")]
    a <- a |> build_stands(idplot = j, data = list(df  = df[[j]],
                                                   data_type = "trees",
                                                   stand_type = "individual",
                                                   date = as.Date("2000-01-01"),
                                                   x = runif(1)),
                           verbose = F)
  }


  # Seedlings.
  seedlings$n <- seedlings$n/3
  for (j in idplot) {
    z <- seedlings[seedlings$idplot == j, c("species", "n")]
    if (nrow(z) > 0) {
      a <- build_stands(a, j, list(df = z,
                                   data_type = "seedlings",
                                   stand_type = "individual",
                                   date = as.Date("2000-01-01")))
    }
  }


  # Saplings.
  for (j in idplot) {
    z <- saplings[saplings$idplot == j, c("species", "n")]
    if (nrow(z) > 0) {
      a <- build_stands(a, j, list(df = z,
                                   data_type = "saplings",
                                   stand_type = "individual",
                                   date = as.Date("2000-01-01")))
    }
  }


  # Get species and check them out.
  b <- calc_species(a, verbose = F)
  expect_true(all(attr(b, "species") %in% c("Pinus nigra", "Pinus halepensis", "Quercus ilex")))
  expect_true(all(c("Pinus nigra", "Pinus halepensis", "Quercus ilex") %in% attr(b, "species")))

})
