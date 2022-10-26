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
#' a <- add_data_stand(a,"c",df,"Spain","trees")
#' a <- update_inventory(a, "c")
#'
update_description <- function(a, idplot = NULL) {

  if (!is.null(idplot)) {
    id <- a$idplot %in% idplot
    if (!any(id)) stop("Could not find 'idplot' stand in inventory 'a'")
    id <- which(id)
  } else {
    id <- 1:length(a$idplot)
  }

  country <- tolower(attr(a, "country"))
  if (!any(tolower(country) %in% c("spain", "usa", "france"))) stop("Wrong value in attribute 'country'")

  for (i in id) {
    df <- a[i,]$trees[[1]]
    if (!is.null(df)) {
      if (a[i,]$stand_type == "individual") {
        if (any(country == "spain")) {
          df <- df %>% group_by(species)
          a[i,]$species[[1]] <- (df %>% distinct(species))$species
          a[i,]$BA_species[[1]] <- as.data.frame(df %>% summarise(BA=(pi/200^2)*sum(factor_diam1*dbh1^2)))
          a[i,]$N_species[[1]] <- as.data.frame(df %>% summarise(N=sum(factor_diam1)))
        } else if (country == "usa") {
        } else if (country == "france") {
        }
      }
      if (a[i,]$stand_type == "mpm") {
      }
      if (a[i,]$stand_type == "ipm") {

        # Retreived only if needed, and only once.
        if (!exists("x", inherits = F)) {
          x <- fpm::get_integvar(a)
          h <- x[2,]-x[1,]
        }
        coln <- colnames(df)
        a[i,]$species[[1]] <- coln
        a[i,]$BA_species[[1]] <-
          data.frame(species = coln, BA = unname(sapply(coln, function(y) MiscMath::quad_ext_simpson(df[,y]*x[,y]^2,h[y]))*(pi/40000)))
        a[i,]$N_species[[1]] <-
          data.frame(species = coln, N = unname(sapply(coln, function(y) MiscMath::quad_ext_simpson(df[,y],h[y]))))
      }
      a[i,]$BA_stand <- sum(a[i,]$BA_species[[1]]$BA)
      a[i,]$N_stand <- sum(a[i,]$N_species[[1]]$N)
    }
  }

  return(a)
}
