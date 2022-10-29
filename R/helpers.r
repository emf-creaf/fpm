# Nulls to NA.
null_to_NA <- function(x) ifelse(is.null(x), NA, x)

# Set how many different types of seedlings.
num_seedlings <- function() 1:3

# Set how many different types of saplings.
num_saplings <- function() 1:3

# Set how many time points (for the Spanish IFN, there are IFN2, IFN3 and IFN4; that is, a maximum of 1:3)
num_timepoints <- function() 1:2

# Is the country correct?
test_country <- function(x) {
  y <- any(tolower(x) %in% c("spain", "usa", "france"))
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









