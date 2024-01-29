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
  source("./R/fpm_ingrowth.r")
  source("./R/clear_stands.r")






  # First initialize stands for the Spanish IFN.
  a <- start_stands(paste0("ID", 1:100), x = runif(100), y = runif(100), "EPSG:4326")
  a <- set_parameters(a, country = "spain")


  # Now we add tree information.
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
  }


  # Add saplings information.
  a <- build_stand(a, "ID3",
                    data.frame(species = c("Pinus nigra", "Pinus halepensis"), n = c(30, 51)),
                    data_type = "saplings",
                    stand_type = "individual",
                    date = as.Date("2000-01-01"),
                    country = "spain")


  # Setting parameters and converting to continuous.
  x <- list('Pinus nigra' = seq(7.5,200,length=1000), 'Pinus halepensis' = seq(7.5,250,length=1500))
  a <- set_parameters(a, integvars = x)
  a <- a |> smooth_stands(verbose = F)

  load("C:\\Roberto\\Ecosystem Modelling Facility\\IPM\\Nuevos ajustes funciones fpm\\Ingrowth model V4.Rdata")


  df <- as.data.frame(ingrowth_model$`Quercus ilex`$model)
  df$ba <- df$q <- df$`log(nseedlings + 1)` <- df$`log(nsaplings + 1)` <- NULL

  df$nseedling <- NULL
  df$nsapling <- NULL
  df$ba <- NULL



  # # Trick.
  df <- df[1:100, ]
  df$idplot <- a$idplot


  # Bigger arrays.
  # a2 <- rbind(a, a)
  # for (i in 1:4) a2 <- rbind(a2, a2)
  # df2 <- rbind(df, df)
  # for (i in 1:4) df2 <- rbind(df2, df2)
  # a2$idplot <- df2$idplot <- paste0("ID", 1:nrow(a2))


  models_list <- list(ingrowth_lambda = ingrowth_lambda,
                      ingrowth_model = ingrowth_model)
  b <- fpm_ingrowth(a, df, models_list, verbose = T)
  expect_true(length(b[3, ]$trees[[1]]) > 0)
  expect_true(all(sapply((1:nrow(a))[-3], function(i) length(b[i, ]$trees[[1]]) == 0)))





})
