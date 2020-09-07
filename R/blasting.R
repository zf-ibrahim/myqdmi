#Blasting

blasting <- function(quarryInput, particleSize = "tsp", areaBlasted = quarryInput$areaBlasted,
                   blastFreq = quarryInput$blastingFreq, blastHole.depth = quarryInput$blasthole, moistureContent = quarryInput$moisture)

{
  quarryInput <- quarryInput

  if(particleSize == "tsp") {
    coeb = 1
  }else if (particleSize == "pm10"){
    coeb = 0.52
  }





  ((344*((areaBlasted^0.8)/((moistureContent^1.9)*(blastHole.depth^1.8))))*coeb)*blastFreq*12/1000


}
