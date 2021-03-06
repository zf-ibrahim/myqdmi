% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/eddm.R
\name{source.model}
\alias{source.model}
\title{Source model for dust deposited rate estimation}
\usage{
source.model(
  sourceInput,
  receptorInput,
  windInput,
  sourceActivity,
  vg = 5,
  dustGenerated = 0.731952,
  AnemometerHeight = 16
)
}
\arguments{
\item{sourceInput}{A data frame containing \code{sourceActivity} is a name for the pointsource, \code{type} (the type of material processing),
\code{x} for Easting, \code{y} for Northing and \code{z} for elevation height of the location.}

\item{receptorInput}{A data frame containing \code{receptor} the name/label for receptor point,
\code{x} for Easting, \code{y} for Northing and \code{z} for elevation height of the location.}

\item{windInput}{A data frame containing \code{ws} for windspeed (m/s) and \code{wd} for wind direction}

\item{sourceActivity}{The pointsource name such as \sQuote{primaryCrusher} or \sQuote{point A} which is
according to the \code{souceInput} \code{sourceActivity}.}

\item{vg}{A constant numeric value for settling velocity. The default value for source model is 5 m/s.}

\item{dustGenerated}{A numeric value of estimation dust generation at a point source based on the site activity. To estimate \code{dustGenerated}
at a point source either use function of \code{blasting}, \code{drilling}, \code{pavedRoad}, \code{pavedRoad}, \code{unpavedRoad},
\code{primaryCrusher}, \code{secondaryCrusher}, \code{tertiaryCrusher}, \code{screening}, or \code{stockpile}.}

\item{AnemometerHeight}{Height of anemometer installation from sea level.}
}
\value{
The results will be the estimation of dust deposited rate at all the receptors from a pointsource.
The unit measurement for dust deposited rate is ug/m2/month.
}
\description{
\code{source.model} Estimation of Dust Deposition Model (EDDM) established to estimate dust deposited rate at receptor
}
\details{
\code{source.model} require the source and receptor location in Easting and Northing format.
The \code{sourceActivity} for this model is the type of material processing activity in the quarry or
if the \code{sourceInput} is the quarries location, the \code{sourceActivity} is the name of the quarries.
}
\examples{

#demo
source.model(sourceInput, receptorInput, windInput, sourceActivity = "primaryCrusher", vg = 5, dustGenerated = 0.731952, AnemometerHeight = 16)
}
\author{
Zul Fadhli & Dr Izhar Abadi
}
