#Distance

#x = SQRT((PcoorX-RcoorX)^2+(PcoorY-RcoorY)^2+(PcoorZ-RcoorZ)^2)

#can use this package
#library(geosphere)
#distm (c(lon1, lat1), c(lon2, lat2), fun = distHaversine)

##' Distance
##'
##' @param sourceInput A data frame containing \code{sourceActivity} is a name for the pointsource, \code{type} (the type of material processing),
##' \code{x} for Easting, \code{y} for Northing and \code{z} for elevation height of the location.
##' 
##' @param receptorInput A data frame containing \code{receptor} the name/label for receptor point,
##' \code{x} for Easting, \code{y} for Northing and \code{z} for elevation height of the location.
##' 
##' @param sourceActivity 
##'
##' @return distance between receptor location to source location
##' @export
##'
##' @examples
##' #demo
##' distance(sourceInput, receptorInput, sourceActivity)
distance <- function(sourceInput, receptorInput, sourceActivity ){

  sourceInput <- sourceInput
  receptorInput <- receptorInput
  sourceActivity <- sourceActivity


  jarak <- rep(NA,nrow(receptorInput))

  for (i in 1:nrow(receptorInput)){
 jarak[i] <- sqrt(
      ((sourceInput[sourceInput$sourceActivity == sourceActivity,"x"] - receptorInput[i,"x"])^2) +
      ((sourceInput[sourceInput$sourceActivity == sourceActivity,"y"] - receptorInput[i,"y"])^2) +
      ((sourceInput[sourceInput$sourceActivity == sourceActivity,"z"] - receptorInput[i,"z"])^2)

     )

  }
  jarak
}
