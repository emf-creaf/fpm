test_that("Checking plots", {


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
  a <- NULL
  for (j in idplot) {
    df[[j]] <- trees[trees$idplot == j, c("dbh", "species")]
    a <- a |> build_stands(idplot = j, df  = trees[trees$idplot == j, c("dbh", "species")],
                          data_type = "trees",
                          stand_type = "individual",
                          date = as.Date("2000-01-01"),
                          country = "spain")
  }


  # Seedlings.
  seedlings$n <- seedlings$n/3
  for (j in idplot) {
    z <- seedlings[seedlings$idplot == j, c("species", "n")]
    if (nrow(z) > 0) {
      a <- build_stands(a, j, df = z,
                       data_type = "seedlings",
                       stand_type = "individual",
                       date = as.Date("2000-01-01"),
                       country = "spain")
    }
  }


  # Saplings.
  for (j in idplot) {
    z <- saplings[saplings$idplot == j, c("species", "n")]
    if (nrow(z) > 0) {
      a <- build_stands(a, j, df = z,
                       data_type = "saplings",
                       stand_type = "individual",
                       date = as.Date("2000-01-01"),
                       country = "spain")
    }
  }


  a <- set_parameters(a, country = "spain")

  # Everything is ok.
  expect_true(all(sapply(1:length(a$idplot), function(i) check_stand(a[i, ]))))


  # Wrong stand_type value.
  b <- a
  b$stand_type <- 98
  expect_warning(check_stand(b[1, ]))

  # Wrong element in column of lists.
  b <- a
  b[25, ]$saplings[[1]] <- 7
  expect_warning(check_stand(b[25, ]))

  # Wrong column names.
  b <- a
  colnames(b[10, ]$trees[[1]]) <- c("rt", "dbh")
  expect_warning(check_stand(b[10, ]))


})
