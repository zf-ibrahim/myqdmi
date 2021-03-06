% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/elevation.R
\name{elevation}
\alias{elevation}
\title{Elevation}
\usage{
elevation(location = receptorInput)
}
\arguments{
\item{location}{A data frame must contain column of "lat" for latitude and "long" for longitude. Value format in decimal.}
}
\description{
To estimate elevation using Shuttle Radar Topography Mission digital elevation model. (SRTM3 data)
}
\examples{
#demo
elevation(location = receptorInput) 
}
\author{
Zul Fadhli & Dr Izhar Abadi
}
