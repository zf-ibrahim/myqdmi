##' Function for Limestone Receptor Model
##'
##' \code{receptorModel.limestone} Multimaviate model established to estimate dust deposited rate at receptor
##' 
##' \code{receptorModel.limestone} require the source and receptor location in Easting and Northing format.
##' The \code{sourceActivity} for this model is the type of material processing activity in the quarry or
##' if the \code{sourceInput} is the quarries location, the \code{sourceActivity} is the name of the quarries.
##' 
##' @param sourceInput A data frame containing \code{sourceActivity} is a name for the pointsource, \code{type} (the type of material processing),
##' \code{x} for Easting, \code{y} for Northing and \code{z} for elevation height of the location.
##' 
##' @param receptorInput A data frame containing \code{receptor} the name/label for receptor point,
##' \code{x} for Easting, \code{y} for Northing and \code{z} for elevation height of the location.
##' 
##' @param windInput A data frame containing \code{ws} for windspeed (m/s) and \code{wd} for wind direction
##' 
##' @param sourceActivity The pointsource name such as \sQuote{primaryCrusher} or \sQuote{point A} which is
##' according to the \code{souceInput} \code{sourceActivity}.
##' 
##' @export
##' 
##' @return The results will be the estimation of dust deposited rate at all the receptors from a pointsource.
##' The unit measurement for dust deposited rate is ug/m2/month.
##' 
##' @author Zul Fadhli & Dr. Izhar Abadi
##' 
##' @examples 
##' 
##' #demo
##' receptorModel.limestone(sourceInput, receptorInput, windInput, sourceActivity = "primaryCrusher", anemometerHeight = 16)
##' 
receptorModel.limestone <- function(sourceInput, receptorInput, windInput, sourceActivity = "primaryCrusher", anemometerHeight = 10) {
  
  sourceName <- sourceActivity
  
  sourceInput <- sourceInput
  receptorInput <- receptorInput
  windInput <- windInput
  
  aws <- mean(windInput$ws, na.rm = T)
  
  wss <- aws * (receptorInput$z/anemometerHeight)
  
  tdh <- quarryInput$drillhole
  
  aba <- quarryInput$areaBlasted
  
  dsr <- distance(sourceInput, receptorInput, sourceActivity = sourceName)
  
  3.306 + 0.0005 * ((aba + tdh)/2) - 0.2818 * ((aws + wss)/2) + 684.95 * (1/dsr)
}
