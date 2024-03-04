test_that("full fpm model", {



  source("./R/start_stands.r")
  source("./R/set_parameters.r")
  source("./R/build_stands.r")
  source("./R/smooth_stands.r")
  source("./R/helpers.r")
  source("./R/factor_diam_IFN.r")
  source("./R/get_parameters.r")
  source("./R/kernsmooth.r")
  source("./R/get_stats.r")
  source("./R/calc_ba.r")
  source("./R/calc_ntrees.r")
  source("./R/get_species.r")
  source("./R/dtrexp.r")
  source("./R/clear_stands.r")
  source("./R/numquad_vm.r")
  #
  source("./R/fpm_elements.r")
  source("./R/fpm_seedlings.r")
  source("./R/fpm_saplings.r")
  source("./R/fpm_ingrowth.r")
  source("./R/fpm_survival.r")
  source("./R/fpm_growth.r")
  source("./R/fpm_quadrature.r")
  source("./R/fpm.r")
  #
  source("./R/collect_parts.R")



  # Load simulated IFN data.
  # load("..\\..\\data\\trees.Rdata")
  # load("..\\..\\data\\seedlings.Rdata")
  # load("..\\..\\data\\saplings.Rdata")

  load(".\\data\\trees.Rdata")
  load(".\\data\\seedlings.Rdata")
  load(".\\data\\saplings.Rdata")

  # Initialize stands.
  idplot <- unique(trees$idplot)
  i <- match(idplot, trees$idplot)
  n <- length(idplot)
  a <- start_stands()


  # Now we add trees to stands.
  df <- dfseed <- dfsapl <- list()

  for (i in idplot) {
    df[[i]] <- trees[trees$idplot == i, c("species", "dbh")]
    if (nrow(df[[i]]) > 0) {
      a <- build_stands(a, i, data = list(df = df[[i]],
                        data_type = "trees",
                        stand_type = "individual",
                        date = as.Date("2000-01-01")), verbose = F)
    }

    dfseed[[i]] <- seedlings[seedlings$idplot == i, c("species", "n")]
    if (nrow(dfseed[[i]]) > 0) {
      dfseed[[i]]$n <- dfseed[[i]]$n/3
      a <- build_stands(a, i, data = list(df = dfseed[[i]],
                       data_type = "seedlings",
                       stand_type = "individual",
                       date = as.Date("2000-01-01")))
    }

    dfsapl[[i]] <- saplings[saplings$idplot == i, c("species", "n")]
    if (nrow(dfsapl[[i]]) > 0) {
      a <- build_stands(a, i, data = list(df = dfsapl[[i]],
                       data_type = "saplings",
                       stand_type = "individual",
                       date = as.Date("2000-01-01")))
    }
  }


  # Setting parameters and converting to continuous.
  x <- list('Pinus nigra' = seq(7.5,200,length=1000),
            'Quercus ilex' = seq(7.5,250,length=1500),
            'Pinus halepensis' = seq(7.5,270,length=1500))
  a <- a|> set_parameters(param = list(integvars = x)) |> smooth_stands(verbose = F)

  load("C:\\Roberto\\Ecosystem Modelling Facility\\IPM\\Nuevos ajustes funciones fpm\\Seedling models V9.Rdata")
  load("C:\\Roberto\\Ecosystem Modelling Facility\\IPM\\Nuevos ajustes funciones fpm\\Sapling models V9.Rdata")
  load("C:\\Roberto\\Ecosystem Modelling Facility\\IPM\\Nuevos ajustes funciones fpm\\Ingrowth models V4.Rdata")
  load("C:\\Roberto\\Ecosystem Modelling Facility\\IPM\\Nuevos ajustes funciones fpm\\Survival models V1.Rdata")
  load("C:\\Roberto\\Ecosystem Modelling Facility\\IPM\\Nuevos ajustes funciones fpm\\Growth models V1.Rdata")

  library(dplyr)
  library(broom)

  df <- growth_model[["Quercus ilex"]] |> augment()
  df$.fitted <- df$.resid <- NULL


  # # Trick.
  df <- df[1:100, ]
  df$idplot <- a$idplot


  a <- a |> set_parameters(param = list(integvars = x, max_dbh = lapply(x, max), min_dbh = lapply(x, min)))

  models_list <- list(seedlings_model = seedlings_model,
                      saplings_model = saplings_model,
                      ingrowth_model = ingrowth_model,
                      ingrowth_lambda = ingrowth_lambda,
                      growth_model = growth_model,
                      variance_model = variance_model,
                      survival_model = survival_model)

  b1 <- b2 <- list()
  b1[[1]] <- a
  b2[[1]] <- 0
  for (i in 2:20) {
    b1[[i]] <- fpm(b1[[i-1]], data = list(df = df, models_list = models_list), verbose = T)
    # b2[[i]] <- fpm_copia(b1[[i-1]], df, models_list, verbose = T)
  }



})
