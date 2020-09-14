##' @export
##'

elevation <- function(location = receptorInput){


x<- location$x

y<- location$y




options(geonamesUsername="ctpp")

storage.vector <- rep(NA,nrow(location))
for (i in 1:nrow(location)){

  storage.vector[i] <- GNsrtm3(lat = location[i,"x"], location[i,"y"])


}

z <- as.data.frame(storage.vector)

colnames(z) <- NULL

z <- t(z)

colnames(z) <- c("height")

z

}
