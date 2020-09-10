##' @export
# Dust Emission from tertiary crusher




#tertiary crusher TSP & PM10 (Year)
#Emission Factor TSP for tertiary crusher is 0.03
#Emission Factor PM10 for tertiary crusher is 0.0012

tertiaryCrusher <- function(quarryInput, particleSize = "tsp", QuarryProduction = quarryInput$quarryProduction,
                             crusherCount = quarryInput$nTertiary)

{
  quarryInput <- quarryInput

  if(particleSize == "tsp") {
    particleSize = 0.0027
  }else if (particleSize == "pm10"){
    particleSize = 0.0012
  }


  (particleSize*(QuarryProduction*8*26)/crusherCount)*12/1000

}








