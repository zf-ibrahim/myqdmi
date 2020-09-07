#Distance

#x = SQRT((PcoorX-RcoorX)^2+(PcoorY-RcoorY)^2+(PcoorZ-RcoorZ)^2)

#can use this package
#library(geosphere)
#distm (c(lon1, lat1), c(lon2, lat2), fun = distHaversine)

distance <- function(sourceInput, receptorInput, sourceActivity = "primaryCrusher"){

  sourceInput <- sourceInput
  receptorInput <- receptorInput


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
