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
  source("./R/fpm_young.r")


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

  load("C:\\Roberto\\Ecosystem Modelling Facility\\IPM\\Nuevos ajustes funciones fpm\\Young models V7.Rdata")

  df <- as.data.frame(saplings_model$`Quercus ilex`$model)
  df$nsaplings_IFN3 <-  df$ba <- df$`log(ntrees_species + 1)` <- df$`log(nseedlings + 1)` <- df$`log(nsaplings + 1)` <- NULL


  # Trick.
  df <- df[1:100, ]
  df$idplot <- a$idplot

  # Bigger arrays.
  # a2 <- rbind(a, a)
  # for (i in 1:4) a2 <- rbind(a2, a2)
  # df2 <- rbind(df, df)
  # for (i in 1:4) df2 <- rbind(df2, df2)
  # a2$idplot <- df2$idplot <- paste0("ID", 1:nrow(a2))
  #
  # attr(a2, "integvars") <- attr(a, "integvars")
  # attr(a2, "h") <- attr(a, "h")

  models_list <- list(seedlings_model = seedlings_model,
                      saplings_model = saplings_model)

  b <- fpm_young(a, df, models_list, verbose = T)



})
