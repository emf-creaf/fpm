% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/clear_stands.R
\name{clear_stands}
\alias{clear_stands}
\title{Create a new \code{sf} stand object.}
\usage{
clear_stands(a)
}
\arguments{
\item{a}{a \code{sf} object containing a number of POINT geometry types.}
}
\value{
Same input \code{sf} object with the same parameters (aka attributes) but
with fields removed or emptied.
}
\description{
It creates a new \code{sf} stand object that is identical to the input, but
with fields empty or NULL, save for "idplot" and "geometry", which are kept.
Attributes, on the other hand, are also kept.
}
\details{
Additional details...
}
\examples{
# First initialize stands.
a <- start_stands()
maxdbh <- list('Pinus halepensis' = 200, 'Pinus nigra' = 230)
a <- set_parameters(a, param = list(maxdbh = maxdbh, crs =  "EPSG:4326"))

# Next, we add one stand.
df <- data.frame(species = c('Pinus halepensis', 'Quercus ilex'), dbh = c(8.6, 12.7))
a <- build_stand(a, "id1", data = list(df = df), verbose = T)

b <- clear_stands(a)
}
