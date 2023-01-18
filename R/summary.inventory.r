#' Title
#'
#' @param a
#' @param ...
#'
#' @return
#'
#' @examples
#' a <- start_inventory(letters[1:5],runif(5), runif(5),"EPSG:4326","Spain")
#' a <- set_type(a)
#' for (i in 1:5) {
#' df <- data.frame(species = c(sample(c("Pnigra","Phalep"),5,replace=T)),dbh = 7.5+runif(5)*20, factor_diam = sample(c(127.324, 31.83099),5,replace=T))
#' a <- add_stand_data(a,letters[i],df,"Spain","trees")
#' }
#' a <- update_inventory(a)
#' summary(a)
#'
summary <- function(a, ...) {
  UseMethod("summary", a)
}

summary.inventory <- function(a, ...) {

  out <- list(
    Num_stands = nrow(a),
    N_species = as.data.frame(bind_rows(a$N_species) %>% group_by(species) %>% summarize(N=sum(N))),
    BA_species = as.data.frame(bind_rows(a$BA_species) %>% group_by(species) %>% summarize(BA=sum(BA))),
    N_total = sum(a$N_stand),
    BA_total = sum(a$BA_stand))

  return(out)
}
