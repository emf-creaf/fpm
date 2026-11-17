test_that("Checking attributes", {



  source("./R/start_stands.R")
  source("./R/set_parameters.R")
  source("./R/build_stands.R")
  source("./R/smooth_stands.R")
  source("./R/factor_diam_IFN.r")
  source("./R/get_parameters.r")
  source("./R/kernsmooth.r")
  source("./R/convolve_uniform.r")
  source("./R/calc_stats.r")
  source("./R/calc_descriptive.r")
  source("./R/calc_species.r")
  source("./R/dtrexp.r")
  source("./R/clear_stands.r")
  source("./R/numquad_vm.r")
  source("./R/rep_dataframe.r")
  #

  source("./R/integvars.R")




  # Load simulated IFN data.
  data(climateSpain)
  data("maxdbh")
  data(seedlings)
  data(saplings)
  data(treesIFN)
  trees <- treesIFN


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



  # Setting parameters and converting to continuous.
  mindbh <- maxdbh
  mindbh[] <- 7.5
  x <- integvars(mindbh, maxdbh, by = .1)
  a <- a |>
    set_parameters(param = list(integvars = x, mindbh = mindbh, maxdbh = maxdbh))




})
