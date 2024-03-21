test_that("full fpm model", {



  source("./R/start_stands.R")
  source("./R/set_parameters.R")
  source("./R/build_stands.R")
  source("./R/smooth_stands.R")
  source("./R/factor_diam_IFN.r")
  source("./R/get_parameters.r")
  source("./R/kernsmooth.r")
  source("./R/calc_stats.r")
  source("./R/calc_descriptive.r")
  source("./R/calc_species.r")
  source("./R/dtrexp.r")
  source("./R/clear_stands.r")
  source("./R/numquad_vm.r")
  source("./R/rep_dataframe.r")
  #
  source("./R/fpm_small.r")
  source("./R/fpm_elements.r")
  source("./R/fpm_survival.r")
  source("./R/fpm_growth.r")
  source("./R/fpm_quadrature.r")
  source("./R/quadrature.r")
  source("./R/fpm.r")
  source("./R/dln.R")
  #
  source("./R/collect_parts.R")



  # Load simulated IFN data.
  # load("..\\..\\data\\trees.Rdata")
  # load("..\\..\\data\\seedlings.Rdata")
  # load("..\\..\\data\\saplings.Rdata")

  load(".\\data\\trees.Rdata")
  load(".\\data\\seedlings.Rdata")
  load(".\\data\\saplings.Rdata")
  load(".\\data\\maxdbh.Rdata")
  load(".\\data\\climateSpain.Rdata")



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
    dfseed[[i]] <- dfseed[[i]] |> dplyr::summarise(n = mean(n, na.rm = T), .by = species)
    if (nrow(dfseed[[i]]) > 0) {
      dfseed[[i]]$n <- dfseed[[i]]$n/3
      a <- build_stands(a, i, data = list(df = dfseed[[i]],
                       data_type = "seedlings",
                       stand_type = "individual",
                       date = as.Date("2000-01-01")))
    }

    dfsapl[[i]] <- saplings[saplings$idplot == i, c("species", "n")]
    dfsapl[[i]] <- dfsapl[[i]] |> dplyr::summarise(n = mean(n, na.rm = T), .by = species)
    if (nrow(dfsapl[[i]]) > 0) {
      a <- build_stands(a, i, data = list(df = dfsapl[[i]],
                       data_type = "saplings",
                       stand_type = "individual",
                       date = as.Date("2000-01-01")))
    }
  }

  load("C:\\Roberto\\Ecosystem Modelling Facility\\IPM\\Nuevos ajustes funciones fpm\\Seedling models V9.Rdata")
  load("C:\\Roberto\\Ecosystem Modelling Facility\\IPM\\Nuevos ajustes funciones fpm\\Sapling models V9.Rdata")
  load("C:\\Roberto\\Ecosystem Modelling Facility\\IPM\\Nuevos ajustes funciones fpm\\Ingrowth models V6.Rdata")
  load("C:\\Roberto\\Ecosystem Modelling Facility\\IPM\\Nuevos ajustes funciones fpm\\Survival models V1.Rdata")
  load("C:\\Roberto\\Ecosystem Modelling Facility\\IPM\\Nuevos ajustes funciones fpm\\Growth models V3.Rdata")


  # Setting parameters and converting to continuous.
  x <- list()
  for (i in names(maxdbh)) {
    x[[i]] <- seq(7.5, maxdbh[i], by = 0.1)
  }
  a <- a |> set_parameters(param = list(integvars = x)) |> smooth_stands()


  # Add idplot to dataset.
  i <- match(a$idplot, climateSpain$idplot)
  a <- a[!is.na(i), ]
  climateSpain <- climateSpain[i[!is.na(i)], ]
  climateSpain$idplot <- as.character(climateSpain$idplot)


  a <- a |> set_parameters(param = list(integvars = x, maxdbh = lapply(x, max), mindbh = lapply(x, min)))

  models <- list(seedlings = seedlings_model,
                      saplings = saplings_model,
                      ingrowth = ingrowth_model,
                      lambda = ingrowth_lambda,
                      growth = growth_model,
                      variance = variance_model,
                      survival = survival_model)


  b1 <- fpm(a, data = climateSpain, models = models, verbose = T, update = T)
  b2 <- fpm(b1, data = climateSpain, models = models, verbose = T, update = T)


})
