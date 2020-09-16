##' coordiante to UTM format
##' @export

coord2utm <- function(location = sourceInput){
  
  x <- location$lat
  y <- location$long
  
  cord.dec <- SpatialPoints(cbind(y, x), proj4string = CRS("+proj=longlat"))
  
  
  
  cord.UTM <- spTransform(cord.dec, CRS("+init=epsg:3375")) #malaysia GDM2000
  
  utm <- as.data.frame(cord.UTM@coords)
  
  colnames(utm) <- c("y", "x")
  
  utm
  
}


