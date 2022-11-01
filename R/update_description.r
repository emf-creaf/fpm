#' Descriptive statistics per tree stand
#'
#' @description
#' Calculate descriptive statistics (number of trees, basal area) at tree-stand level per species
#'
#' @param a a \code{sf} object containing a number of POINT geometry types.
#' @param idplot identifiers of POINT elements representing tree-stands. If NULL,
#' all plots will be used.
#'
#' @return
#' A \code{sf} object with aggregate values (number of trees, basal area) per
#' tree stand and tree species.
#'
#' @details
#' When the tree-stand information is given as a continuous distribution, amn
#' appropriate numerical quadrature is emmployed. In this case, the \emph{integvars}
#' attribute must be set in advance.
#'
#' @export
#'
#' @examples
#'
#' # First initialize one single stand.
#' a <- start_stand("ID1", 5, 45, "EPSG:4326")
#' a <- set_attributes(a)
#'
#' # Next, we merge other stands.
#' for (i in 2:10) {
#' b <- start_stand(paste0("ID",i), 5, 45, "EPSG:4326")
#' b <- set_attributes(b)
#' a <- merge_stands(a,b)
#' }
#'
#' # Now we add tree information.
#' for (i in 1:10) {
#' df <- data.frame(species = c(sample(c("Pnigra","Phalep"),5,replace=T)),
#' dbh1 = 7.5+runif(5)*20, factor_diam1 = sample(c(127.324, 31.83099),5,replace=T))
#' a <- build_stand(a, paste0("ID",i), df, "trees", "individual", 1990)
#' }
#'
#' # Convolve to obtain a continuous distribution.
#' x <- data.frame(Pnigra = seq(7.5,200,length=1000), Phalep = seq(7.5,250,length=1000))
#' a <- set_attributes(a, integvars = x)
#' b <- smooth_stand(a)
#'
#' # Update descriptive statistics for continuous data.
#' a <- update_description(a)
#' b <- update_description(b)
#'
update_description <- function(a, idplot = NULL) {

  # Check idplot. If not provided, all plots will be used.
  if (!is.null(idplot)) {
    id <- match(idplot, a$idplot)
    if (any(is.na(id))) stop(cat("Could not find some 'idplots' in input 'a'"))
  } else {
    id <- 1:length(a$idplot)
  }

  country <- tolower(attr(a, "country"))
  if (is.null(country)) stop("Attribute 'country' in 'a' has not been set")

  for (i in id) {
    df <- a[i, ]$trees[[1]]
    if (a[i, ]$stand_type == "individual") {
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
        x <- attr(a, "integvars")
        if (is.null(x)) stop("Attribute 'integvars' has not been set")
        h <- unlist(x[2,]-x[1,])
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

  return(a)
}
