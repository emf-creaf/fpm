# Nulls to NA.
null_to_NA <- function(x) ifelse(is.null(x), NA, x)

# Set how many different types of seedlings.
num_seedlings <- function() 1:3

# Set how many different types of saplings.
num_saplings <- function() 1:3

# Set how many time points (for the Spanish IFN, there are IFN2, IFN3 and IFN4; that is, a maximum of 1:3)
num_timepoints <- function() 1:2

countries <- function() c("spain", "usa", "france")

# Is the country correct?
test_country <- function(x) {
  y <- any(tolower(x) %in% countries())
  if (!y) warning("Wrong value in attribute 'country'")
  return(y)
}

# Are seedlings correctly labelled, i.e. seedlings1, seedlings2...
test_seedlings <- function(x, country) {
  county <- tolower(country)
  if (country == "spain") {
    y <- any(x == paste0("seedlings", num_seedlings()))
    if (!y) warning("Wrong seedlings name in input 'data'")
  } else if (country == "france") {
  } else if (country == "usa") {
  }
  return(y)
}

# Are saplings correctly labelled, i.e. saplings1, saplings2...
test_saplings <- function(x, country) {
  county <- tolower(country)
  if (country == "spain") {
    y <- any(x == paste0("saplings", num_saplings()))
    if (!y) warning("Wrong saplings name in input 'data'")
  } else if (country == "france") {
  } else if (country == "usa") {
  }
  return(y)
}

# Do seedlings/saplings have one species per row (unlike trees)?
test_row_species <- function(x) {
  y <- length(unique(x$species)) == nrow(x)
  if (!y) warning("Input data.frame should have only one row per species")
  return(y)
}

# Are trees correctly labelled?
test_trees_labels <- function(x, country) {
  county <- tolower(country)
  if (country == "spain") {
    y <- all(colnames(x) %in% c("species",outer(c("dbh", "factor_diam"), num_timepoints(), "paste0")))
    if (!y) warning("Column names in input data.frame 'df' are wrong")
  } else if (country == "france") {
  } else if (country == "usa") {
  }
    return(y)
}

# Do seedlings/saplings have correct names according to available time points?
test_timepoints_youngs <- function(x, country) {
  county <- tolower(country)
  if (country == "spain") {
    y <- all(colnames(x) %in% c("species", paste0("n",num_timepoints())))
    if (!y) warning("Column names in input seedlings/saplings data.frame are wrong")
  }
  return(y)
}

# Descriptive statistics for seedlings or saplings.
stat_minor_trees <- function(a) {
  species <- unique(unlist(sapply(a, function(x) unique(x$species))))
  species_number <- sapply(a, function(x) sum(!is.na(unique(x$species))))
  species_NA <- sapply(a, function(x) sum(is.na(x$species)))
  number <- sapply(a, function(x) sum(x$N, na.rm=T))
  number_NA <- sapply(a, function(x) sum(is.na(x$N)))
  return(list(
    species = species,
    species_number = species_number,
    species_NA = species_NA,
    number = number,
    number_NA = number_NA
  ))
}


# Quadrature for single species.
quad_growth <- function(N, x, mean_growth, sd_growth, prob_surv, meandbh,
                        country = c("spain"), quadrature = c("trapez", "simpson")) {

  country <- match.arg(country)
  quadrature <- match.arg(quadrature)

  if (country == "spain") {
    # Big matrix for growth term.
    nx <- nrow(x)
    gmat <- matrix(NA, nx, nx)
    xx <- x - mindbh
    jseq <- 1:nx
    for (j in 1:nx) {
      gmat[j, jseq] <- dlnorm(xx, meanlog = mean_growth[j, i], sdlog = sd_growth[j,i])
      xx <- xx[-length(xx)]
      jseq <- jseq[-1]
    }

    # Numerical quadrature with trapezoidal rule.
    b[, i] <- numquad_vm(N * prob_surv, gmat, nx, "simpson")
  }

  return(b)
}

find2 <- function(x, y) {
  i <- match(x, names(y))
  return(ifelse(is.na(i), 0, unlist(y[i])))
}


# Find index.
frsf <- function(df, namecol, value) which(df[[namecol]] == value)


# If stand_type is "individual", makes sure that the input data.frame
# has the right columns depending on the country of origin.
check_individual_type <- function(df, country) {
  colnames(df) <- tolower(colnames(df))
  if (country == "spain") {

    stopifnot("Input 'df' has wrong column names" = all(c("species", "dbh", "factor_diam") %in% colnames(df)))
  } else if (country == "usa") {

  } else if (country == "france") {

  }
  return(df)
}





# Functions to check lower or upper bounds in "assert" call.
is_larger <- function(bound, include.bound = F) {
  the_call <- deparse(sys.call())
  fun <- function(x) {
    if (is.null(x)) stop("bounds must be checked on non-null element")
    if (!is.numeric(x)) stop("bounds must only be checked on numerics")
    operator <- if (!include.bound) `>` else `>=`
    return(operator(x, bound) & !is.na(x))
  }

  attr(fun, "assertr_vectorized") <- TRUE
  attr(fun, "call") <- the_call
  return(fun)
}

is_smaller <- function(bound, include.bound = F) {
  the_call <- deparse(sys.call())
  fun <- function(x) {
    if (is.null(x)) stop("bounds must be checked on non-null element")
    if (!is.numeric(x)) stop("bounds must only be checked on numerics")
    operator <- if (!include.bound) `<` else `<=`
    return(operator(x, bound) & !is.na(x))
  }

  attr(fun, "assertr_vectorized") <- TRUE
  attr(fun, "call") <- the_call
  return(fun)
}




quad <- function(y, h, type = 1) {

  if (!is.vector(y) & !is.matrix(y)) stop("y must be a vector or a matrix")
  if (!any(type %in% c(1, 2))) stop("Input 'type' must be equal to 1 or 2")

  ny <- ifelse(is.vector(y),length(y),nrow(y))

  if (type == 1) {
    if (ny < 6) stop("y must have length (if it is a vector) or number of rows (if it is a matrix) equal to or larger than 6")
    if (is.vector(y)) {
      q <- sum(y) - 5/8*(y[1]+y[ny]) + 1/6*(y[2]+y[ny-1]) - 1/24*(y[3]+y[ny-2])
    } else {
      q <- colSums(y) - 5/8*(y[1,]+y[ny,]) + 1/6*(y[2,]+y[ny-1,]) - 1/24*(y[3,]+y[ny-2,])
    }
  } else {
    if (ny < 8) stop("y must have length (if it is a vector) or number of rows (if it is a matrix) equal to or larger than 8")
    if (is.vector(y)) {
      q <- sum(y) - 31/48*(y[1]+y[ny]) + 11/48*(y[2]+y[ny-1]) - 5/48*(y[3]+y[ny-2]) + 1/48*(y[4]+y[ny-3])
    } else {
      q <- colSums(y) - 31/48*(y[1,]+y[ny,]) + 11/48*(y[2,]+y[ny-1,]) - 5/48*(y[3,]+y[ny-2,]) + 1/48*(y[4,]+y[ny-3,])
    }
  }

  return(h*q)

}


# NO FUNCIONA. log(x) inside drule[["dlnorm"]] is FALSE(X) for some reason.
# drule[["dlnorm"]] <- make_drule()
# ff <- Deriv(function(x, m, s) dlnorm(x, m, s), "x", drule = drule)
# x <- seq(0,4,length=1000)
# plot(diff(dlnorm(x))/(x[2]-x[1]))
# points(ff(x, 0, 1),type="l")

make_drule <- function() {
  alist(x = -(sdlog^2 + log(x) - meanlog)/(x * sdlog^2) * dlnorm(x, meanlog, sdlog),
        meanlog = (log(x) - meanlog)/sdlog^2 * dlnorm(x, meanlog, sdlog),
        sdlog =(meanlog^2 + log(x)^2 - sdlog^2 - 2*meanlog*log(x))/sdlog^3 * dlnorm(x, meanlog, sdlog))

}



# rep_dataframe <- function(df, n) {
#   # as.data.frame(lapply(df, rep, n))
#   # df[rep(seq_len(nrow(df)), n), ]
#   z <- setNames(data.frame(matrix(0, n, ncol(df))), colnames(df))
#   z[] <- df
#   return(z)
# }



# Little function seen in https://stackoverflow.com/questions/11121385/repeat-rows-of-a-data-frame
# and needed below.
rep_dataframe <- function(x, times) as.data.frame(lapply(x, rep, times))

