#screen

screening <- function(quarryInput, particleSize = "tsp", QuarryProduction = quarryInput$quarryProduction,
                      totalScreen = quarryInput$nScreen)

{
  quarryInput <- quarryInput

  if(particleSize == "tsp") {
    efc = 0.008
  }else if (particleSize == "pm10"){
    efc = 0.0043
  }



  efc*QuarryProduction*8*26*12/totalScreen/1000


}
