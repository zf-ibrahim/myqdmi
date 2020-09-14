##' BRT model for DDR
##' @export

model.ai <- function(quarryInput, windInput, sourceInput, receptorInput, sourceActivity = "primaryCrusher"){

quarryInput <- quarryInput
windInput <- windInput
sourceInput <- sourceInput
receptorInput <- receptorInput

sourceActivity <- sourceActivity

dist <- distance(sourceInput, receptorInput, sourceActivity = sourceActivity)

pred <- data.frame(dist = dist)

a <- quarryInput$quarryProduction

pred$BlastingFreq <- quarryInput$blastingFreq

pred$AveBlastingArea <- quarryInput$areaBlasted

pred$DrillHole <- quarryInput$drillhole

pred$UnpavedRoadDist <- quarryInput$unpavedDist

pred$NumOfVehicle <- quarryInput$unpavedVehicle


pred$AveWindSpeed <- mean(windInput$ws, na.rm = T)

pred$CalmCond <- calmFreq(windInput)



pred$wcount <- length(windInput$ws)

pred$AveWndSpeedStabF <- windAvg(windInput, stability = "F")

pred$AveWndSpeedStabA <- windAvg(windInput, stability = "A")

pred$Highdiff <- receptorInput$z - sourceInput$z

pred$wcount_Calm <- calmFreq(windInput, calm = T)


windFreq2 <- function(windInput, sourceInput, receptorInput, sourceActivity = "primaryCrusher"){
  
  sourceInput <- sourceInput
  receptorInput <- receptorInput
  sourceActivity = sourceActivity
  
  
  
  
    compassName <- ifelse(windInput$wd=="NA", compass <- "NA",
                        ifelse(windInput$wd>337.4, compass <- "N",
                               ifelse(windInput$wd>292.4, compass <- "NW",
                                      ifelse(windInput$wd>247.4, compass <- "W",
                                             ifelse(windInput$wd>202.4, compass <- "SW",
                                                    ifelse(windInput$wd>157.4, compass <- "S",
                                                           ifelse(windInput$wd>112.4, compass <- "SE",
                                                                  ifelse(windInput$wd>67.4, compass <- "E",
                                                                         ifelse(windInput$wd>22.4, compass <- "NE",
                                                                                ifelse(windInput$wd>=0, compass <- "N", compass <- "error"))))))))))
  
  windInput <- cbind(windInput, compass = compassName)
  
  
  
  
  storage.vector <- rep(NA,nrow(windInput))
  
  for (i in 1:nrow(windInput)){
    #  storage.vector[i] <- windInput[i+1,1] - windInput[i,1]
    
    storage.vector[i] <-ifelse(windInput[i+1,2]=="NA", obangle <- "NA",
                               ifelse(windInput[i+1,2]-windInput[i,2],(abs(windInput[i+1,2]-windInput[i,2])),
                                      ifelse(windInput[i+1,2]-windInput[i,2]==0,0)))
    
  }
  
  
  
  
  
  storage.vector <- ifelse(storage.vector== "NA", ocAngle <- "NA",
                           ifelse(storage.vector>180,360-storage.vector,storage.vector))
  
  
  windOutput <- data.frame(compass = windInput[2:nrow(windInput),4])
  
  #argument for windClass & stability & windDirection
  
  direction <- bearing(sourceInput, receptorInput, sourceActivity = sourceActivity)
  
  direction <- as.data.frame(direction)
  
  storage.vector <- rep(NA,NROW(direction))
  
  for( i in 1:NROW(direction)){
  
    storage.vector[i] <- length(which(windInput$compass == direction[i,"direction"]))
    
    
    
  }
  
  storage.vector
  
}

pred$PercentWindBlowToReseptor <- windFreq2(windInput, sourceInput, receptorInput, sourceActivity = sourceActivity)





predict(DDR.gbm, pred, n.trees = 1384)


}

