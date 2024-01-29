test_that("Sapling model", {



  source("./R/start_stands.r")
  source("./R/set_parameters.r")
  source("./R/build_stand.r")
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

  source("./R/fpm_seedling.r")
  source("./R/fpm_sapling.r")
  source("./R/fpm_ingrowth.r")
  source("./R/fpm_survival.r")
  source("./R/fpm_growth.r")
  source("./R/fpm_quadrature.r")
  source("./R/fpm.r")
  source("./R/fpm_copia.r")

  source("./R/collect_parts.R")





  # First initialize stands for the Spanish IFN.
  a <- start_stands(paste0("ID", 1:100), x = runif(100), y = runif(100), "EPSG:4326")
  a <- a|> set_parameters(country = "spain")


  # Now we add tree and sapling information.
  df <- list()
  for (i in 1:100) {
    dbh <- 7.5+runif(5)*20
    df[[i]] <- data.frame(species = c(sample(c("Pinus nigra","Pinus halepensis"),5,replace=T)),
                          dbh = dbh, factor_diam = factor_diam_IFN(dbh, "area"))
    a <- build_stand(a, paste0("ID",i), df[[i]],
                      data_type = "trees",
                      stand_type = "individual",
                      date = as.Date("2000-01-01"),
                      country = "spain")

    a <- build_stand(a, paste0("ID",i),
                     data.frame(species = c("Pinus nigra", "Pinus halepensis"), n = runif(2)),
                     data_type = "seedlings",
                     stand_type = "individual",
                     date = as.Date("2000-01-01"),
                     country = "spain")

    a <- build_stand(a, paste0("ID",i),
                     data.frame(species = c("Pinus nigra", "Pinus halepensis"), n = sample(1:50, 2)),
                     data_type = "saplings",
                     stand_type = "individual",
                     date = as.Date("2000-01-01"),
                     country = "spain")
  }




  # Setting parameters and converting to continuous.
  x <- list('Pinus nigra' = seq(7.5,200,length=1000), 'Pinus halepensis' = seq(7.5,250,length=1500))
  a <- set_parameters(a, integvars = x) |> smooth_stands(verbose = F)

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


  a <- a |> set_parameters(integvars = x) |> set_parameters(country = "spain") |>
    set_parameters(max_dbh = lapply(x, max)) |> set_parameters(min_dbh = lapply(x, min))

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
    b1[[i]] <- fpm(b1[[i-1]], df, models_list, verbose = T)
    # b2[[i]] <- fpm_copia(b1[[i-1]], df, models_list, verbose = T)
  }



})
