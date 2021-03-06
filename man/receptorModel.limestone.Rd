% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/limestoneReceptorModel.R
\name{receptorModel.limestone}
\alias{receptorModel.limestone}
\title{Function for Limestone Receptor Model}
\usage{
receptorModel.limestone(
  sourceInput,
  receptorInput,
  windInput,
  sourceActivity = "primaryCrusher",
  anemometerHeight = 10
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
}
\value{
The results will be the estimation of dust deposited rate at all the receptors from a pointsource.
The unit measurement for dust deposited rate is ug/m2/month.
}
\description{
\code{receptorModel.limestone} Multimaviate model established to estimate dust deposited rate at receptor
}
\details{
\code{receptorModel.limestone} require the source and receptor location in Easting and Northing format.
The \code{sourceActivity} for this model is the type of material processing activity in the quarry or
if the \code{sourceInput} is the quarries location, the \code{sourceActivity} is the name of the quarries.
}
\examples{

#demo
receptorModel.limestone(sourceInput, receptorInput, windInput, sourceActivity = "primaryCrusher", anemometerHeight = 16)

}
\author{
Zul Fadhli & Dr. Izhar Abadi
}
