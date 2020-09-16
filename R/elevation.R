##' @export
##'

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
