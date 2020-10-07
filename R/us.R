##' Ground level wind speed at point source
##' 
##' A function to estimate wind speed at ground level of selected point source
##' 
##' 
##' @param sourceInput A data frame containing \code{sourceActivity} is a name for the pointsource, \code{type} (the type of material processing),
##' \code{x} for Easting, \code{y} for Northing and \code{z} for elevation height of the location.
##' 
##' @param windInput A data frame containing \code{ws} for windspeed (m/s) and 
##' \code{wd} for wind direction
##'
##' @param sourceActivity Select the point source labeled in the \code{sourceInput} dataframe. For example; 
##' The pointsource name such as \sQuote{primaryCrusher} or \sQuote{point A} which is according to the \code{souceInput} \code{sourceActivity}.
##' 
##' @param AnemometerHeight Height of anemometer installation from sea level.
##' 
##' @param stability Six category stability which is 'A', 'B', 'C', 'D', 'E', and 'F'.
##' 
##' @param windClass Five wind band which is > 5.67 m/s, > 3.61 m/s, > 2.07 m/s, 0.52 and < 0.51
##' characterized as 'class1', 'class2', 'class3', 'class4' and 'calm' respectively
##' 
##' @export
##' 
##' @author Zul Fadhli & Dr. Izhar Abadi
##' 
##' @example 
##' #demo
##' us <- function(sourceInput, windInput, sourceActivity = "primaryCrusher", AnemometerHeight = 2, stability = "A", windClass = "class1")




us <- function(sourceInput, windInput, sourceActivity = "primaryCrusher", AnemometerHeight = 2, stability = "A", windClass = "class1"){

  sourceInput <- sourceInput

  windInput <- windInput
  
  sourceActivity <- sourceActivity

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







  if(stability == "A") {
    stability = 0.15
  }else if (stability == "B"){
    stability = 0.15
  }else if (stability == "C"){
    stability = 0.2
  }else if (stability == "D"){
    stability = 0.25
  }else if (stability == "E"){
    stability = 0.4
  }else if (stability == "F"){
    stability = 0.6
  }


  if(windClass == "class1") {
    uasub<-subset(windInput, wsclass == "class1")
    ua <- mean(uasub[,1])
  }else if (windClass == "class2") {
    uasub<-subset(windInput, wsclass == "class2")
    ua <- mean(uasub[,1])
  }else if (windClass == "class3") {
    uasub<-subset(windInput, wsclass == "class3")
    ua <- mean(uasub[,1])
  }else if (windClass == "class4") {
    uasub<-subset(windInput, wsclass == "class4")
    ua <- mean(uasub[,1])
  }
    if(is.nan(ua)) {ua <- 0}

  #Us = Ua * (PcoorZ/Za)^p

  ua*(sourceInput[sourceInput$sourceActivity == sourceActivity,"z"]/AnemometerHeight)^stability

}
