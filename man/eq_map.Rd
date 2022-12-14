% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/map_geom_functions_week3.R
\name{eq_map}
\alias{eq_map}
\title{Creates a leaflet map of earthquakes}
\usage{
eq_map(data, annot_col)
}
\arguments{
\item{data}{A data frame containing cleaned NOAA earthquake data}

\item{annot_col}{A character. The name of the column in the data that should
be used as descriptor.}
}
\value{
A leaflet map with earthquakes and annotations.
}
\description{
This function creates a \code{leaflet} map of selected earthquakes based on
input NOAA earthquake cleaned data.
}
\examples{
\dontrun{
eq_map(data, annot_col = "LOCATION_NAME")
}
}
