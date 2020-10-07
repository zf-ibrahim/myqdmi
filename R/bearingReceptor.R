##' Bearing of receptor from point source
##' 
##' 
##' \code{bearing} A function to determine the direction of receptor location to source location.
##' 
##' \code{bearing} require the source and receptor location in Easting and Northing format.
##' The \code{sourceActivity} for this model is the type of material processing activity in the quarry or
##' if the \code{sourceInput} is the quarries location, the \code{sourceActivity} is the name of the quarries.
##' 
##' @param sourceInput A data frame containing \code{sourceActivity} is a name for the pointsource, \code{type} (the type of material processing),
##' \code{x} for Easting, \code{y} for Northing and \code{z} for elevation height of the location.
##' 
##' @param receptorInput A data frame containing \code{receptor} the name/label for receptor point,
##' \code{x} for Easting, \code{y} for Northing and \code{z} for elevation height of the location.
##' 
##' @param sourceActivity The pointsource name such as \sQuote{primaryCrusher} or \sQuote{point A} which is
##' according to the \code{souceInput} \code{sourceActivity}.
##' 
##' @export
##' 
##' @return The value return to the direction of receptor point to the source location
##' 
##' @author Zul Fadhli & Dr Izhar Abadi
##' 
##' @examples 
##' #demo
##' bearing(sourceInput = sourceInput, receptorInput = receptorInput, sourceActivity = "primaryCrusher")
##' 
bearing <- function(sourceInput, receptorInput, sourceActivity = "primaryCrusher"){
  
  sourceInput <- sourceInput
  receptorInput <- receptorInput
  sourceActivity <- sourceActivity
  

  jx <- rep(NA,nrow(receptorInput))
  jy <- rep(NA,nrow(receptorInput))


  for (i in 1:nrow(receptorInput)){

  jx[i] <- sourceInput[sourceInput$sourceActivity == sourceActivity,"x"] - receptorInput[i,"x"]
  jy[i] <- sourceInput[sourceInput$sourceActivity == sourceActivity,"y"] - receptorInput[i,"y"]


  }



#  if(atan(jy/jx)*180/3.142 < 0) {
#    bearingDegree <- -1*(atan(jy/jx)*180/3.142)
#  }else {
#    bearingDegree <- atan(jy/jx)*180/3.142
#  }

  ifelse(atan(jy/jx)*180/3.142 < 0,
    bearingDegree <- -1*(atan(jy/jx)*180/3.142),
    bearingDegree <- atan(jy/jx)*180/3.142
  )


         bearingDegree <- abs(bearingDegree)


  #C = IF(AND(AND(Jx<0,Jy<0),AND(Ɵ>0,Ɵ<90)),"NE",
  #IF(OR(AND(Jx>0,Jy<0),AND(Ɵ>90,Ɵ<180)),"NW",
  #IF(AND(AND(Jx>0,Jy>0),AND(Ɵ>0,Ɵ<90)),"SW",
  #IF(AND(AND(Jx<0,Jy>0),AND(Ɵ>0,Ɵ<90)),"SE",
  #IF(OR(Ɵ=0,Ɵ=360),"E",
  #IF(Ɵ=90,"N",
  #IF(Ɵ=180,"W",
  #IF(Ɵ=270,"S","FALSE"))))))))




  ifelse( ((jx < 0) & (jy < 0)) & ((bearingDegree > 0) & (bearingDegree < 90)),
    bearingReceptor <- "NE",
    ifelse( ((jx > 0) & (jy < 0)) | ((bearingDegree > 90) & (bearingDegree < 180)) ,
    bearingReceptor <- "NW",
  ifelse( ((jx > 0) & (jy > 0)) & ((bearingDegree > 0) & (bearingDegree < 90)) ,
    bearingReceptor <- "SW",
  ifelse( ((jx < 0) & (jy > 0)) & ((bearingDegree > 0) & (bearingDegree < 90)) ,
    bearingReceptor <- "SE",
  ifelse( (bearingDegree == 0) | (bearingDegree == 360) ,
    bearingReceptor <- "N",
  ifelse( (bearingDegree == 180) ,
    bearingReceptor <- "W",
  ifelse( (bearingDegree == 270) ,
    bearingReceptor <- "S", bearingReceptor <- "please input coordinate correctly"
  )))))))



  #print(bearingReceptor)
}




