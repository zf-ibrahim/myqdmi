##' @export
#wind speed average for stablities and wind wind classes 
#Malaysian quarry dust monitoring artificial intelligence android apps (MyQDMi)


windAvg <- function(windInput, stability = "F"){
  
  
  
  
  
  storage.vector <- rep(NA,nrow(windInput))
  
  for (i in 1:nrow(windInput)){
    #  storage.vector[i] <- windInput[i+1,1] - windInput[i,1]
    
    storage.vector[i] <-ifelse(windInput[i+1,2]=="NA", obangle <- "NA",
                               ifelse(windInput[i+1,2]-windInput[i,2],(abs(windInput[i+1,2]-windInput[i,2])),
                                      ifelse(windInput[i+1,2]-windInput[i,2]==0,0)))
    
  }
  
  
  
  storage.vector <- ifelse(storage.vector== "NA", ocAngle <- "NA",
                           ifelse(storage.vector>180,360-storage.vector,storage.vector))
  
  wstability <- ifelse(storage.vector=="NA", stabilityClass <- "NA",
                       ifelse(storage.vector>=90, stabilityClass <- "A",
                              ifelse(storage.vector>=40, stabilityClass <- "B",
                                     ifelse(storage.vector>=15, stabilityClass <- "C",
                                            ifelse(storage.vector>=0, stabilityClass <- "F", stabilityClass <- "error")))))
  
  stab <- data.frame(wstability = wstability)
  
  windOutput <- data.frame(wstability = stab[1:nrow(stab)-1,1])
  
  missing <- data.frame(wstability = NA)
  
  windOutput <- rbind(missing, windOutput)
  
  windOutput <- cbind(windOutput, ws = windInput$ws)
  
  #argument for windClass & stability & windDirection
  
  mean(windOutput[windOutput$wstability == stability, "ws"], na.rm =  TRUE)
  
  
}
