##' coordiante to UTM format
##' 
##' \code{coord2utm} Converting lat long into Northing and Easting. Using epsg 3375 GDM2000 (Malaysia) for map projection and datum transformation
##' 
##' @param location A data frame must contain column of "lat" for latitude and "long" for longitude. Value format in decimal.
##'
##' @export
##' 
##' @author Zul Fadhli & Dr Izhar Abadi
##' 
##' @examples  
##' #demo
##' coord2utm(location = sourceInput)

coord2utm <- function(location = sourceInput){
  
  x <- location$lat
  y <- location$long
  
  cord.dec <- SpatialPoints(cbind(y, x), proj4string = CRS("+proj=longlat"))
  
  
  
  cord.UTM <- spTransform(cord.dec, CRS("+init=epsg:3375")) #malaysia GDM2000
  
  utm <- as.data.frame(cord.UTM@coords)
  
  colnames(utm) <- c("y", "x")
  
  utm
  
}


