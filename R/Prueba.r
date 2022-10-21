library(dplyr)

a <- start_inventory(letters[1:5],runif(5),runif(5),rep("individual",5),runif(5),"EPSG:4326")

a <- set_attributes(a)

for (i in letters[1:5]) {
  df <- data.frame(species = c(sample(c("Pnigra","Phalep"),5,replace=T)),dbh = 7.5+runif(5)*20, factor_diam = sample(c(127.324, 31.83099),5,replace=T))
  a <- add_data_stand(a, i, df, "trees")
}

a <- update_inventory(a)

# Add stands. Stand "e" is changed.
for (i in paste0("ID",1:1000)) {
  b <- start_inventory(i,runif(1),runif(1),"individual",runif(1),"EPSG:4326")
  df <- data.frame(species = c(sample(c("Pnigra","Phalep"),5,replace=T)),dbh = 7.5+runif(5)*20, factor_diam = sample(c(127.324, 31.83099),5,replace=T))
  b <- add_data_stand(b, i, df)
  b <- set_attributes(b)
  a <- add_stand(a, i, b)
}

a <- update_inventory(a)

print(summarize_inventory(a))



x <- seq(7.5, 300 ,length = 2000)
for (i in a$idplot) {
  a <- smooth_stand(a, i, x)
}





