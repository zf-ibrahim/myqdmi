##' @export
#Bearing of receptor from point source
#Ɵ =IF((
#  ATAN(Jy/Jx)*180/3.142)<0,-1*(
#    ATAN(Jy/Jx)*180/3.142),(
#     ATAN(Jy/Jx)*180/3.142))


#C = IF(AND(AND(Jx<0,Jy<0),AND(Ɵ>0,Ɵ<90)),"NE",
#IF(OR(AND(Jx>0,Jy<0),AND(Ɵ>90,Ɵ<180)),"NW",
#IF(AND(AND(Jx>0,Jy>0),AND(Ɵ>0,Ɵ<90)),"SW",
#IF(AND(AND(Jx<0,Jy>0),AND(Ɵ>0,Ɵ<90)),"SE",
#IF(OR(Ɵ=0,Ɵ=360),"E",
#IF(Ɵ=90,"N",
#IF(Ɵ=180,"W",
#IF(Ɵ=270,"S","FALSE"))))))))



bearing <- function(sourceInput, receptorInput, sourceActivity = "primaryCrusher"){

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




