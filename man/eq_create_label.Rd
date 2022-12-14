% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/map_geom_functions_week3.R
\name{eq_create_label}
\alias{eq_create_label}
\title{Creates a label for a leaflet map}
\usage{
eq_create_label(data)
}
\arguments{
\item{data}{A data frame containing cleaned NOAA earthquake data}
}
\value{
A character vector with labels
}
\description{
This function creates a label for the \code{leaflet} map based on location
name, magnitude and casualties from NOAA earthquake data
}
\details{
The input \code{data.frame} requires the following columns:
LOCATION_NAME, EQ_PRIMARY and TOTAL_DEATHS with the earthquake location, magintude and
total casualties respectively.
}
\examples{
\dontrun{
eq_create_label(data)
}
}
