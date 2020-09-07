#Drilling

drilling <- function(quarryInput, particleSize = "tsp",
                   drillHole = quarryInput$drillhole)

{
  quarryInput <- quarryInput

  if(particleSize == "tsp") {
    efd = 0.59
  }else if (particleSize == "pm10"){
    efd = 0.31
  }



  (efd*drillHole)/1000*12

}
