#' Title
#'
#' @param a
#' @param ...
#'
#' @return
#' @export
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
summarize_inventory <- function(a) {

  out <- list(Country = attr(a, "country"),
              Num_stands = nrow(a),
              Num_stand_types = table(a$stand_type))

  # Group by "individual", "mpm", "ipm"

  # for (i in 1:length(a$idplot)) {
  #   if (a[i,]$stand_type == "individual") {
  #     out$N_species = bind_rows(a$N_species) %>% group_by(species) %>% summarize(N=sum(N))
  #     out$BA_species = bind_rows(a$BA_species) %>% group_by(species) %>% summarize(BA=sum(BA))
  #     out$N_total = sum(a$N_stand)
  #     out$BA_total = sum(a$BA_stand)
  #   } else if (a$stand_type[i] == "mpm") {
  #   } else if (a$stand_type[i] == "ipm") {
  #   }
  # }

  return(out)
}
