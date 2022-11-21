test_that("Remove stands from 'sf' object", {

  # First initialize one single stand.
  a <- start_stand("ID1", 5, 45, "EPSG:4326")
  a <- set_attributes(a)

  # Next, we merge other stands.
  for (i in paste0("ID",2:10)) {
    b <- start_stand(i, 5, 45, "EPSG:4326")
    b <- set_attributes(b)
    a <- merge_stands(a,b)
  }

  # Now we add tree information.
  for (i in paste0("ID",1:10)) {
    df <- data.frame(species = c(sample(c("Pnigra","Phalep"),5,replace=T)),
                     dbh1 = 7.5+runif(5)*20, factor_diam1 = sample(c(127.324, 31.83099),5,replace=T))
    a <- build_stand(a, i, df, "trees", "individual", 1990)
  }

  # Remove two stands.
  a <- remove_stand(a, c("ID3", "ID8"))

  # There must be 8 rows now.
  expect_equal(nrow(a), 8)

  # Check classes.
  expect_identical(class(a), c("sf", "data.frame"))

  # Check reference system.
  expect_identical(sf::st_crs(a), sf::st_crs("EPSG:4326"))

  # Check colnames.
  expect_true(all(colnames(a) %in% c("idplot","geometry","trees","seedlings",
                                  "saplings","N_species","BA_species","species",
                                  "N_stand","BA_stand", "data_type", "stand_type", "date")))

})
