##' @export
#wind Frequency PTG(MS excel)
#Malaysian quarry dust monitoring artificial intelligence android apps (MyQDMi)


windFreq <- function(windInput, stability = "F", windClass = "class1", windDirection = "NE"){


  windBand <- ifelse(windInput$ws > 8.75,
                     band <- "class1",
                     ifelse(windInput$ws > 5.67,
                            band <- "class1",
                            ifelse(windInput$ws > 3.61,
                                   band <- "class2",
                                   ifelse(windInput$ws > 2.07,
                                          band <- "class3",
                                          ifelse(windInput$ws > 0.52,
                                                 band <- "class4",
                                                 ifelse(windInput$ws >= 0,
                                                        band <- "calm", band <- "NA")))))
  )


  windInput <- cbind(windInput, wsclass = windBand)


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

 wstability <- ifelse(storage.vector=="NA", stabilityClass <- "NA",
                     ifelse(storage.vector>=90, stabilityClass <- "A",
                            ifelse(storage.vector>=40, stabilityClass <- "B",
                                   ifelse(storage.vector>=15, stabilityClass <- "C",
                                          ifelse(storage.vector>=0, stabilityClass <- "F", stabilityClass <- "error")))))
 stab <- data.frame(wstability = wstability)

 windOutput <- data.frame(class = windInput[2:nrow(windInput),3] , wstability = stab[1:nrow(stab)-1,1], compass = windInput[2:nrow(windInput),4])

 #argument for windClass & stability & windDirection

 length(which(windOutput$class == windClass &
                windOutput$wstability == stability &
                windOutput$compass == windDirection))

}
