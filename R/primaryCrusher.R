# Dust Emission from Primary crusher



#Primary crusher TSP & PM10 (Year)
#Emission Factor TSP for Primary crusher is 0.01
#Emission Factor PM10 for Primary crusher is 0.004

primaryCrusher <- function(quarryInput, particleSize = "tsp", QuarryProduction = quarryInput$quarryProduction,
                              crusherCount = quarryInput$nPrimary)

{
  quarryInput <- quarryInput

  if(particleSize == "tsp") {
    particleSize = 0.01
  }else if (particleSize == "pm10"){
    particleSize = 0.004
  }


  (particleSize*(QuarryProduction*8*26)/crusherCount)*12/1000

  }








