##' Elevation
##' 
##' To estimate elevation using Shuttle Radar Topography Mission digital elevation model. (SRTM3 data)
##' 
##' @param location A data frame must contain column of "lat" for latitude and "long" for longitude. Value format in decimal.
##'
##' @export
##' 
##' @author Zul Fadhli & Dr Izhar Abadi
##' 
##' @examples 
##' #demo
##' elevation(location = receptorInput) 


elevation <- function(location = receptorInput){


x<- location$lat

y<- location$long




options(geonamesUsername="ctpp")

storage.vector <- rep(NA,NROW(location))
for (i in 1:nrow(location)){

  storage.vector[i] <- GNsrtm3(lat = location[i,"lat"], location[i,"long"])


}

z <- as.data.frame(storage.vector)

colnames(z) <- NULL

z <- t(z)

colnames(z) <- c("height")

z

}
