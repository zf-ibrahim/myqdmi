#Stockpile

stockpile <- function(quarryInput, particleSize = "tsp", QuarryProduction = quarryInput$quarryProduction,
                            mositure = quarryInput$moisture)

{
  quarryInput <- quarryInput

  if(particleSize == "tsp") {
    ks = 0.74
  }else if (particleSize == "pm10"){
    ks = 0.35
  }

  avgws <- mean(windInput$ws)

  (ks*0.0016*((avgws/2.2)^1.3)/((mositure/2)^1.4))*QuarryProduction*8*26*12/1000


}
