test_that("Building tree stands", {


  # source("./R/start_stands.R")
  # source("./R/build_stand.R")
  # source("./R/set_parameters.R")
  # source("./R/build_stands.R")
  # source("./R/smooth_stands.R")
  # source("./R/factor_diam_IFN.r")
  # source("./R/get_parameters.r")
  # source("./R/kernsmooth.r")
  # source("./R/calc_stats.r")
  # source("./R/calc_descriptive.r")
  # source("./R/calc_species.r")
  # source("./R/dtrexp.r")
  # source("./R/clear_stands.r")
  # source("./R/numquad_vm.r")
  # source("./R/rep_dataframe.r")
  # #
  # source("./R/fpm_small.r")
  # source("./R/fpm_elements.r")
  # source("./R/fpm_survival.r")
  # source("./R/fpm_growth.r")
  # source("./R/fpm_quadrature.r")
  # source("./R/quadrature.r")
  # source("./R/fpm.r")
  # source("./R/dln.R")
  # source("./R/integvars.R")
  # #
  # source("./R/collect_parts.R")



  # Load simulated IFN data.
  load("..\\..\\data\\IFNtrees.Rdata")
  load("..\\..\\data\\IFNseedlings.Rdata")
  load("..\\..\\data\\IFNsaplings.Rdata")

  # Load simulated IFN data.
  # load(".\\data\\IFNtrees.Rdata")
  # load(".\\data\\IFNseedlings.Rdata")
  # load(".\\data\\IFNsaplings.Rdata")

  # Initialize sf object and add 50 stands.
  idplot <- unique(c(trees$idplot, seedlings$idplot, saplings$idplot))
  i <- match(idplot, trees$idplot)
  n <- length(idplot)

  df <- list()
  a <- start_stands()
  for (j in idplot) {
    df[[j]] <- trees[trees$idplot == j, c("dbh", "species")]
    a <- a |> build_stand(idplot = j, data = list(df  = df[[j]],
                     data_type = "trees",
                     stand_type = "individual",
                     date = as.Date("2000-01-01")),
                     verbose = F)
  }


  # Seedlings.
  seedlings$n <- seedlings$n/3
  for (j in idplot) {
    z <- seedlings[seedlings$idplot == j, c("species", "n")]
    if (nrow(z) > 0) {
      a <- build_stand(a, j, list(df = z,
                       data_type = "seedlings",
                       stand_type = "individual",
                       date = as.Date("2000-01-01")))
    }
  }


  # Saplings.
  for (j in idplot) {
    z <- saplings[saplings$idplot == j, c("species", "n")]
    if (nrow(z) > 0) {
      a <- build_stand(a, j, list(df = z,
                       data_type = "saplings",
                       stand_type = "individual",
                       date = as.Date("2000-01-01")))
    }
  }


  # Tree data have been successfully saved in 'a'.
  check <-  T
  for (j in idplot) {
    check <- check & identical(a[a$idplot==j,]$trees[[1]], df[[j]][c("species", "dbh")])
  }
  expect_true(check)


  # Check classes.
  expect_identical(class(a), c("sf", "data.frame"))


  # Check data.frame class.
  check <-  T
  for (j in idplot) {
    check <- check & identical(class(a[a$idplot==j,]$trees[[1]]), "data.frame")
  }
  expect_true(check)


  # Check stand_type.
  check <-  T
  for (i in idplot) {
    check <- check & (a[a$idplot==j,]$stand_type == "individual")
  }
  expect_true(check)

  # We add a new stand and check that it is ok.
  a <- build_stand(a, idplot = "id200", data = list(df = a[1, ]$trees[[1]],
                   data_type = "trees",
                   stand_type = "individual",
                   date = as.Date("2000-01-01")),
                   verbose = F)
  df[["id200"]] <- a[1, ]$trees[[1]]
  expect_identical(a[a$idplot == "id200",]$trees[[1]], df[["id200"]][c("species", "dbh")])


  # Fails when seedlings or saplings are negative.
  expect_error(a <- build_stand(a, "id3",
                                data = list(df = data.frame(species = c("Pinus nigra", "Pinus halepensis"), n = -c(1, 1)),
                                data_type = "saplings",
                                stand_type = "individual",
                                date = as.Date("2000-01-01")), verbose = F))


  expect_error(a <- build_stand(a, "ID2",
                                data = list(df = data.frame(species = c("Pinus nigra", "Pinus halepensis"), n = - runif(2)),
                                data_type = "seedlings",
                                stand_type = "individual",
                                date = as.Date("2000-01-01")), verbose = F))

})
