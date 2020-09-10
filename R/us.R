##' @export
#halaju angin pada ketinggian sesuatu punca

#Us = Ua * (PcoorZ/Za)^p


us <- function(sourceInput, windInput, sourceActivity = "primaryCrusher", AnemometerHeight = 2, stability = "A", windClass = "class1"){

  sourceInput <- sourceInput

  windInput <- windInput

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



  ua*(sourceInput[sourceInput$sourceActivity == sourceActivity,"z"]/AnemometerHeight)^stability

}
