#' Title
#'
#' @param a
#'
#' @return
#' @export
#'
#' @examples
#' a <- start_inventory(letters[1:5],runif(5), runif(5),"EPSG:4326","Spain")
#' df <- data.frame(species = c(sample(c("Pnigra","Phalep"),5,replace=T)),dbh = 7.5+runif(5)*20, factor_diam = sample(c(127.324, 31.83099),5,replace=T))
#' a <- add_stand_data(a,"c",df,"Spain","individual","trees")
#' a <- update_inventory(a, "c")
#'
update_inventory <- function(a, idplot = NULL) {

  if (!is.null(idplot)) {
    id <- a$idplot %in% idplot
    if (!any(id)) stop("Could not find 'idplot' stand in inventory 'a'")
    id <- which(id)
  } else {
    id <- 1:length(a$idplot)
  }

  type <- attr(a, "type")
  country <- tolower(attr(a, "country"))

  for (i in id) {
    df <- a[i,]$trees[[1]]
    if (!is.null(df)) {
      if (type == "individual") {
        if (any(country == "spain")) {
          df_sp <- df %>% group_by(species)
          a[i,]$species[[1]] <- (df_sp %>% distinct(species))$species
          a[i,]$BA_species[[1]] <- as.data.frame(df_sp %>% summarise(BA=(pi/200^2)*sum(factor_diam*dbh^2)))
          a[i,]$N_species[[1]] <- as.data.frame(df_sp %>% summarise(N=sum(factor_diam)))
          a[i,]$BA_stand <- sum(a[i,]$BA_species[[1]]$BA)
          a[i,]$N_stand <- sum(a[i,]$N_species[[1]]$N)
        }
        if (country == "usa") {
        }
        if (country == "france") {
        }
      }
      if (type == "matrix") {
      }
      if (type == "continuous") {
      }
    }
  }

  return(a)
}
