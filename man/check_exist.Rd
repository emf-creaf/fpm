% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_exist.r
\name{check_exist}
\alias{check_exist}
\title{Check the existence of plot}
\usage{
check_exist(a, idplot)
}
\arguments{
\item{a}{a \code{sf} object containing a number of POINT geometry types.}

\item{idplot}{identifier of single POINT, representing a tree stand, to modify.}
}
\value{
\code{check_exist} returns a named logical vector of the same length as
\code{idplot} with TRUE or FALSE depending on whether or not \code{idplot} values
match those in the \code{idplot} value of \code{a}.
}
\description{
\code{check_exist} checks the existence of plots within a \code{data.frame},
\code{tibble} or \code{sf} objects by matching the \code{idplot} field.
}
\examples{
a <- start_stand(paste0("ID", 1:10), runif(10), runif(10), "EPSG:4326")
check_exist(a, paste0("ID", 3:25))
}
