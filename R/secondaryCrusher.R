##' @export
# Dust Emission from Secondary crusher




#Secondary crusher TSP & PM10 (Year)
#Emission Factor TSP for Secondary crusher is 0.03
#Emission Factor PM10 for Secondary crusher is 0.012

secondaryCrusher <- function(quarryInput, particleSize = "tsp", QuarryProduction = quarryInput$quarryProduction,
                           crusherCount = quarryInput$nSecondary)

{
  quarryInput <- quarryInput

  if(particleSize == "tsp") {
    particleSize = 0.03
  }else if (particleSize == "pm10"){
    particleSize = 0.012
  }


  (particleSize*(QuarryProduction*8*26)/crusherCount)*12/1000

}








